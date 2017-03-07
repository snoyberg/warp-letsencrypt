{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Leader.Consul
  ( mkConsulJudge
  , mkConsulOrInMemoryJudge
  , ConsulSettings (..)
  ) where

import ClassyPrelude.Conduit
import Leader
import Network.HTTP.Simple
import Control.Monad.Trans.Unlift (MonadBaseUnlift)

data ConsulSettings = ConsulSettings
  { consulPrefix :: !Text
  }

mkConsulJudge :: (MonadIO m, MonadIO n)
              => ConsulSettings
              -> m (Judge n state)
mkConsulJudge = undefined

mkConsulOrInMemoryJudge
  :: (MonadIO m, MonadIO n, MonadBaseUnlift IO n)
  => ConsulSettings
  -> m (Judge n state)
mkConsulOrInMemoryJudge prefix = do
  consulRunning <- checkConsul
  if consulRunning then mkConsulJudge prefix else mkInMemoryJudge

checkConsul :: MonadIO m => m Bool
checkConsul = liftIO $ do
  eres <- tryAny $ httpLBS $ setRequestMethod "HEAD" "http://localhost:8500/v1/status/leader"
  return $ case eres of
    Right res -> getResponseStatusCode res == 200
    Left _ -> False
