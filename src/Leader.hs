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

import ClassyPrelude.Conduit

newtype StateId = StateId Word
  deriving Eq

-- | Decides who will be the leader
newtype Judge state = Judge (Follower state -> IO ())

mkTLSJudge :: forall state. IO (Judge state)
mkTLSJudge = do
  leaderBaton <- newMVar ()
  stateVar :: TVar (Maybe (StateId, state)) <- newTVarIO Nothing
  stateIdVar <- newTVarIO 0
  return $ Judge $ \Follower {..} -> do
    let beLeader = withMVar leaderBaton $ const $ do
          mstate <- atomically $ readTVar stateVar
          let setState state = atomically $ do
                next <- readTVar stateIdVar
                writeTVar stateIdVar $! next + 1
                writeTVar stateVar $ Just (StateId next, state)
          runConduit
             $ (followerOnLeader (snd <$> mstate) >>= yield)
            .| awaitForever setState
        beFollower = followerRun $ readTVar stateVar >>= maybe retrySTM return
    runConcurrently $ Concurrently (forever beLeader) *> Concurrently beFollower

type LeaderFunction state = Maybe state -> ConduitM () state IO state
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
    (inner mstate >>= yield) .| debounce
  where
    debounce = await >>= maybe (error "impossible") (\state -> yield state *> loop state)

    loop state1 = await >>= maybe (return state1)
      (\state2 ->
        if state1 == state2
          then loop state1
          else yield state2 >> loop state2)

killableFollower :: (state -> IO ()) -> FollowerFunction state
killableFollower inner getState = do
  atomically getState >>= loop
  where
    loop (stateid, state) =
      join $ withAsync (inner state) $ const $ atomically $ do
        (newid, newstate) <- getState
        checkSTM $ stateid /= newid
        return $ loop (newid, newstate)
