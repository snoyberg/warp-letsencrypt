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
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, Settings, setBeforeMainLoop)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChainMemory)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import System.Process.Typed
import System.Directory (findExecutable, doesFileExist, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import Network.Mime (defaultMimeLookup)
import Network.HTTP.Types (status200, status404)
import System.IO.Temp (createTempDirectory)
import Control.Monad.Trans.Resource (allocate)
import Data.Function (fix)

data LetsEncryptSettings = LetsEncryptSettings
  { lesInsecureSettings :: !Settings
  , lesSecureSettings :: !Settings
  , lesEmailAddress :: !Text
  , lesDomains :: ![Text]
  , lesApp :: !Application
  }

config, work, logs, htdocs :: FilePath
config = "config"
work = "work"
logs = "logs"
htdocs = "htdocs"

data LEState
  = LESChallenge (Map FilePath ByteString)
  | LESCerts ByteString ByteString ByteString
  deriving Eq

runLetsEncrypt :: MonadIO m => LetsEncryptSettings -> m ()
runLetsEncrypt les = liftIO $ runResourceT $ do
  tempDir <- liftIO getTemporaryDirectory
  (releaseKey, rootDir) <- allocate
    (createTempDirectory tempDir "warp-letsencrypt")
    (void . tryIO . removeDirectoryRecursive)

  liftIO $ createDirectoryIfMissing True $ rootDir </> htdocs
  judge <- mkTLSJudge
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
leader LetsEncryptSettings {..} exeName rootDir _ignorePrevState = do
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
    say "verification ready"
    let dir = rootDir </> htdocs </> ""
    files <- sourceDirectoryDeep True dir .| foldMapMC
      (\fp -> do
          suffix <-
            case stripPrefix dir fp of
              Nothing -> error $ "Bad file list: " ++ show (dir, fp)
              Just suffix -> return $ fromMaybe suffix $ stripPrefix "/" suffix
          bs <- readFile fp
          return $ singletonMap suffix bs)
    say $ "found files: " ++ tshow (keys files)
    yield $ LESChallenge files
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
      yield $ LESCerts cert chain privkey

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
follower LetsEncryptSettings {..} (LESChallenge files) =
    liftIO $ runSettings lesInsecureSettings app
  where
    app req send = do
      let path = intercalate "/" $ pathInfo req
          mime = defaultMimeLookup path
      send $ case lookup (unpack path) files of
        Nothing -> responseBuilder status404 [] "Not found"
        Just bs -> responseBuilder status200 [("Content-Type", mime)] (toBuilder bs)
follower LetsEncryptSettings {..} (LESCerts cert chain privkey) =
  liftIO $ runConcurrently $ Concurrently secure *> Concurrently insecure
  where
    secure = do
      let tlsSettings = tlsSettingsChainMemory cert [chain] privkey
      runTLS tlsSettings lesSecureSettings lesApp

    insecure = runSettings lesInsecureSettings $ forceSSL lesApp

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
