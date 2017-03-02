{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Warp.LetsEncrypt
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)

main :: IO ()
main = runLetsEncrypt LetsEncryptSettings
  { lesInsecureSettings = setPort 8080 defaultSettings
  , lesSecureSettings = setPort 8443 defaultSettings
  , lesEmailAddress = "michael@snoyman.com"
  , lesDomains = ["tlstest.snoyman.com", "tlstest.yesodweb.com"]
  , lesApp = dummyApp
  , lesBeforeSecure = return ()
  }

dummyApp :: Application
dummyApp _ send = send $ responseLBS status200 [] "Hello World!"

