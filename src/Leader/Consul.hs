{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Leader.Consul
  ( mkConsulJudge
  , mkConsulOrTLSJudge
  ) where

import ClassyPrelude.Conduit
import Leader
import Network.HTTP.Simple

mkConsulJudge :: (MonadIO m, MonadIO n)
              => Text -- ^ prefix
              -> m (Judge n state)
mkConsulJudge = undefined

mkConsulOrTLSJudge :: (MonadIO m, MonadIO n, MonadBaseUnlift IO n)
                   => Text -- ^ prefix
                   -> m (Judge n state)
mkConsulOrTLSJudge prefix = do
  consulRunning <- checkConsul
  if consulRunning then mkConsulJudge prefix else mkTLSJudge

checkConsul :: MonadIO m => m Bool
checkConsul = liftIO $ do
  eres <- tryAny $ httpNoBody "http://localhost:8500/v1/status/leader"
  return $ case eres of
    Right res -> getResponseStatusCode res == 200
    Left _ -> False
