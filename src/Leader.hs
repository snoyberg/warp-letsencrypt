{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Leader
  ( StateId
  , Judge
  , Follower
  , LeaderFunction
  , FollowerFunction
  , mkTLSJudge
  , mkFollower
  , debounceLeader
  , killableFollower
  , runFollower
  ) where

import ClassyPrelude

newtype StateId = StateId Word
  deriving Eq

-- | Decides who will be the leader
newtype Judge state = Judge (Follower state -> IO ())

mkTLSJudge :: forall state. IO (Judge state)
mkTLSJudge = do
  leaderBaton <- newMVar ()
  stateVar :: TVar (Maybe (StateId, state)) <- newTVarIO Nothing
  return $ Judge $ \Follower {..} -> do
    let beLeader = withMVar leaderBaton $ const $ forever $ do
          mstate <- atomically $ readTVar stateVar
          state <- followerOnLeader $ snd <$> mstate
          let stateId =
                case mstate of
                  Nothing -> StateId minBound
                  Just (StateId sid, _) -> StateId (sid + 1)
          atomically $ writeTVar stateVar $ Just (stateId, state)
        beFollower = followerRun $ readTVar stateVar >>= maybe retrySTM return
    runConcurrently $ Concurrently (forever beLeader) *> Concurrently beFollower

type LeaderFunction state = Maybe state -> IO state
type FollowerFunction state = STM (StateId, state) -> IO ()

data Follower state = Follower
  { followerOnLeader :: !(LeaderFunction state)
  , followerRun :: !(FollowerFunction state)
  }

runFollower :: Judge state -> Follower state -> IO ()
runFollower (Judge run) = run

mkFollower :: LeaderFunction state
           -> FollowerFunction state
           -> Follower state
mkFollower = Follower

debounceLeader :: Eq state => LeaderFunction state -> LeaderFunction state
debounceLeader inner mstate =
    loop
  where
    loop = do
        state <- inner mstate
        case mstate of
            Just old | old == state -> loop
            _ -> return state

killableFollower :: (state -> IO ()) -> FollowerFunction state
killableFollower inner getState = do
  atomically getState >>= loop
  where
    loop (stateid, state) =
      join $ withAsync (inner state) $ const $ atomically $ do
        (newid, newstate) <- getState
        checkSTM $ stateid /= newid
        return $ loop (newid, newstate)
