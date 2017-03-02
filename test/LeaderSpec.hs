{-# LANGUAGE NoImplicitPrelude #-}
module LeaderSpec (spec) where

import Test.Hspec
import ClassyPrelude.Conduit
import Leader

spec :: Spec
spec = do
  describe "TLS sanity checks" $ do
    it "runs the leader once" $ do
      leaderCount <- newTVarIO (0 :: Int)
      followerCount <- newTVarIO (0 :: Int)
      judge <- mkTLSJudge
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
