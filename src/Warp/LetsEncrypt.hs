{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Warp.LetsEncrypt
  ( LetsEncryptSettings(..)
  , runLetsEncrypt
  ) where

import ClassyPrelude.Conduit
import Leader
import Leader.Consul
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, Settings, setBeforeMainLoop)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChainMemory)
import System.Process.Typed
import System.Directory (findExecutable, doesFileExist, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import Network.Mime (defaultMimeLookup)
import Network.HTTP.Types (status200, status404)
import System.IO.Temp (createTempDirectory)
import Control.Monad.Trans.Resource (allocate)
import Data.Function (fix)
import Control.Concurrent.Async.Lifted.Safe (Concurrently (..))

data LetsEncryptSettings = LetsEncryptSettings
  { lesInsecureSettings :: !Settings
  , lesSecureSettings :: !Settings
  , lesEmailAddress :: !Text
  , lesDomains :: ![Text]
  , lesApp :: !Application
  , lesBeforeSecure :: !(IO ())
  }

config, work, logs, htdocs :: FilePath
config = "config"
work = "work"
logs = "logs"
htdocs = "htdocs"

data LEState
  = LESJustInsecure
  | LESChallenge (Map FilePath ByteString) (Maybe CertInfo)
  | LESCerts CertInfo
  deriving Eq

data CertInfo = CertInfo ByteString ByteString ByteString
    deriving Eq

runLetsEncrypt :: MonadIO m
               => Text -- ^ consul prefix
               -> LetsEncryptSettings
               -> m ()
runLetsEncrypt prefix les = liftIO $ runResourceT $ do
  tempDir <- liftIO getTemporaryDirectory
  (releaseKey, rootDir) <- allocate
    (createTempDirectory tempDir "warp-letsencrypt")
    (void . tryIO . removeDirectoryRecursive)

  liftIO $ createDirectoryIfMissing True $ rootDir </> htdocs
  judge <- mkConsulOrTLSJudge prefix
  exeName <- liftIO $ findFirstExe ["letsencrypt", "certbot"]
  runFollower judge $ mkFollower
    (debounceLeader (leader les exeName rootDir))
    (killableFollower (follower les))

leader :: MonadResource m
       => LetsEncryptSettings
       -> FilePath
       -> FilePath
       -> Maybe LEState
       -> ConduitM () LEState m LEState
leader LetsEncryptSettings {..} exeName rootDir mprevState = do
  moldCerts <- case mprevState of
    Just (LESCerts certs) -> return $ Just certs
    Just (LESChallenge _ moldCerts) -> return moldCerts
    Just LESJustInsecure -> return Nothing
    Nothing -> yield LESJustInsecure $> Nothing
  liftIO lesBeforeSecure
  let pc = setStdin closed $ setStderr createSource $ proc exeName
        [ "certonly"
        , "--non-interactive", "--verbose"
        , "--email", unpack lesEmailAddress
        , "--agree-tos", "--webroot"
        , "--config-dir", rootDir </> config
        , "--work-dir", rootDir </> work
        , "--logs-dir", rootDir </> logs
        , "-w", rootDir </> htdocs
        , "-d", unpack $ intercalate "," lesDomains
        ]
  bracketP (startProcess pc) stopProcess $ \p -> do
    filesReady <- liftIO $ newTVarIO False
    let checkFilesReady = fix $ \loop -> do
            mbs <- await
            let ready = atomically $ writeTVar filesReady True
            case mbs of
                Nothing -> ready
                Just bs
                  | "verification" `isInfixOf` bs -> ready
                  | otherwise -> loop
    liftIO $ runConduit
           $ getStderr p
          .| getZipSink
               (ZipSink (linesUnboundedAsciiC .| checkFilesReady))
             -- *> ZipSink stderrC)
    atomically $ readTVar filesReady >>= checkSTM
    let dir = rootDir </> htdocs </> ""
    files <- sourceDirectoryDeep True dir .| foldMapMC
      (\fp -> do
          suffix <-
            case stripPrefix dir fp of
              Nothing -> error $ "Bad file list: " ++ show (dir, fp)
              Just suffix -> return $ fromMaybe suffix $ stripPrefix "/" suffix
          bs <- readFile fp
          return $ singletonMap suffix bs)
    yield $ LESChallenge files moldCerts
    checkExitCode p

    domain <-
      case lesDomains of
        [] -> error "No domains"
        x:_ -> return x
    let readFile' fp = readFile $ rootDir </> config </> "live" </> unpack domain </> fp

    forever $ do
      cert <- readFile' "cert.pem"
      chain <- readFile' "chain.pem"
      privkey <- readFile' "privkey.pem"
      yield $ LESCerts $ CertInfo cert chain privkey

      threadDelay $ 1000 * 1000 * 60 * 60 * 12 -- 12 hours
      runProcess_ $ proc exeName
        [ "renew"
        , "--non-interactive"
        , "--agree-tos"
        , "--config-dir", rootDir </> config
        , "--work-dir", rootDir </> work
        , "--logs-dir", rootDir </> logs
        ]

follower :: MonadIO m => LetsEncryptSettings -> LEState -> m ()
follower LetsEncryptSettings {..} LESJustInsecure =
    liftIO $ runSettings lesInsecureSettings lesApp
follower LetsEncryptSettings {..} (LESChallenge files moldCerts) =
    liftIO $ withSecure $ runSettings lesInsecureSettings app
  where
    withSecure insecure =
      case moldCerts of
        Nothing -> insecure
        Just oldCerts -> runConcurrently
          $ Concurrently insecure
         *> Concurrently (runTLS (mkTlsSettings oldCerts) lesSecureSettings $ addSecureHeader lesApp)
    app req send = do
      let path = intercalate "/" $ pathInfo req
          mime = defaultMimeLookup path
      case lookup (unpack path) files of
        Nothing -> lesApp req send
        Just bs -> send $ responseBuilder status200 [("Content-Type", mime)] (toBuilder bs)
follower LetsEncryptSettings {..} (LESCerts certInfo) =
  liftIO $ runConcurrently $ Concurrently secure *> Concurrently insecure
  where
    secure = runTLS (mkTlsSettings certInfo) lesSecureSettings (addSecureHeader lesApp)

    insecure = runSettings lesInsecureSettings lesApp

mkTlsSettings (CertInfo cert chain privkey) = tlsSettingsChainMemory cert [chain] privkey

addSecureHeader app req send =
    app req' send
  where
    req' = req
        { requestHeaders = ("X-Forwarded-Proto", "https") : requestHeaders req }

findFirstExe :: [String] -> IO FilePath
findFirstExe origs =
  let loop [] =
        error $
        "None of the following executables were found: " ++ intercalate ", " origs
      loop (x:xs) = do
        mres <- findExecutable x
        case mres of
          Nothing -> loop xs
          Just res -> return res
  in loop origs
