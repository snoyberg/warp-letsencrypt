{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module LeaderSpec (spec) where

import Test.Hspec
import ClassyPrelude.Conduit
import Leader
import Leader.Consul

spec :: Spec
spec = do
  describe "In memory sanity checks" $ sanity mkInMemoryJudge
  describe "In memory or Consul sanity checks" $ sanity $ mkConsulOrInMemoryJudge ConsulSettings
    { consulPrefix = "leader-test"
    }

sanity :: (forall a. IO (Judge IO a)) -> Spec
sanity mkJudge = do
    it "runs the leader once" $ do
      leaderCount <- newTVarIO (0 :: Int)
      followerCount <- newTVarIO (0 :: Int)
      judge <- mkJudge
      let follower = mkFollower
            (\mstate ->
               case mstate of
                 Nothing -> atomically $ modifyTVar leaderCount (+ 1)
                 Just () -> forever $ threadDelay maxBound)
            (killableFollower $ \() -> atomically $ modifyTVar followerCount (+ 1))
          run = runFollower judge follower
      withAsync run $ const $ withAsync run $ const $ withAsync run $ const $ do
        atomically $ readTVar followerCount >>= checkSTM . (== 3)
      atomically (readTVar leaderCount) >>= (`shouldBe` 1)
