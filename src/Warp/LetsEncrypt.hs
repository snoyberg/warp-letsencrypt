{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Warp.LetsEncrypt
  ( LetsEncryptSettings(..)
  , runLetsEncrypt
  ) where

import ClassyPrelude
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, Settings, setBeforeMainLoop)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import System.Process.Typed
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (findExecutable, doesFileExist, createDirectoryIfMissing)
import Network.Mime (defaultMimeLookup)
import Network.HTTP.Types (status200)

data LetsEncryptSettings = LetsEncryptSettings
  { lesInsecureSettings :: !Settings
  , lesSecureSettings :: !Settings
  , lesEmailAddress :: !Text
  , lesDomains :: ![Text]
  }

data Env = Env
  { envSettings :: !LetsEncryptSettings
  , envApp :: !Application
  , envRootDir :: !FilePath
  , envExeName :: !FilePath
  , envInsecureRunning :: !(TVar Bool)
  , envCertAvailable :: !(TVar Bool)
  }

runLetsEncrypt :: LetsEncryptSettings -> Application -> IO ()
runLetsEncrypt envSettings envApp =
  withSystemTempDirectory "warp-letsencrypt" $
  \envRootDir -> do
    envExeName <- findFirstExe ["letsencrypt", "certbot"]
    envInsecureRunning <- newTVarIO False
    envCertAvailable <- newTVarIO False
    createDirectoryIfMissing True $ envRootDir </> htdocs
    let env =
          Env
          { ..
          }
    runConcurrently $
      Concurrently (insecure env) <|>
      Concurrently (secure env) <|>
      Concurrently (certbot env)

insecure :: Env -> IO ()
insecure Env {..} =
    runSettings settings insecureApp
  where
    LetsEncryptSettings {..} = envSettings
    settings = setBeforeMainLoop (atomically $ writeTVar envInsecureRunning True) lesInsecureSettings

    insecureApp req send = do
      mpath <-
        if any isUnsafe $ pathInfo req
          then return Nothing
          else do
            let suffix = intercalate "/" $ pathInfo req
                path = envRootDir </> htdocs </> unpack suffix
            exists <- doesFileExist path
            return $
              if exists
                then Just (path, defaultMimeLookup suffix)
                else Nothing
      case mpath of
        Nothing -> forceSSL envApp req send
        Just (path, mime) -> send $ responseFile status200 [("Content-Type", mime)] path Nothing

    isUnsafe :: Text -> Bool
    isUnsafe "." = True
    isUnsafe ".." = True
    isUnsafe "" = True
    isUnsafe t = '/' `elem` t

config, work, logs, htdocs :: FilePath
config = "config"
work = "work"
logs = "logs"
htdocs = "htdocs"

secure :: Env -> IO ()
secure Env {..} = do
  atomically $ readTVar envCertAvailable >>= checkSTM
  domain <-
    case lesDomains of
      [] -> error "No domains"
      x:_ -> return x
  runTLS (tlsSettings domain) lesSecureSettings envApp
  where
    LetsEncryptSettings {..} = envSettings

    tlsSettings domain = tlsSettingsChain
        (root </> "cert.pem")
        [root </> "chain.pem"]
        (root </> "privkey.pem")
      where
        root = envRootDir </> config </> "live" </> unpack (domain :: Text)

certbot :: Env -> IO ()
certbot Env {..} = do
  atomically $ readTVar envInsecureRunning >>= checkSTM
  runProcess_ $ proc envExeName
    [ "certonly"
    , "--email", unpack lesEmailAddress
    , "--agree-tos", "--webroot"
    , "--config-dir", envRootDir </> config
    , "--work-dir", envRootDir </> work
    , "--logs-dir", envRootDir </> logs
    , "-w", envRootDir </> htdocs
    , "-d", unpack $ intercalate "," lesDomains
    ]
  atomically $ writeTVar envCertAvailable True
  forever $ threadDelay maxBound -- FIXME loop delaying and checking for updates
  where
    LetsEncryptSettings {..} = envSettings

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
