{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
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
newtype Judge m state = Judge (Follower m state -> m ())

mkTLSJudge :: forall state m n. (MonadIO m, MonadIO n, MonadBaseUnlift IO n) => m (Judge n state)
mkTLSJudge = liftIO $ do
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
    unlift <- askRunBase
    liftIO $
      runConcurrently $ Concurrently (unlift beLeader)
                     *> Concurrently (unlift beFollower)

type LeaderFunction m state = Maybe state -> ConduitM () state m state
type FollowerFunction m state = STM (StateId, state) -> m ()

data Follower m state = Follower
  { followerOnLeader :: !(LeaderFunction m state)
  , followerRun :: !(FollowerFunction m state)
  }

runFollower :: Judge m state -> Follower m state -> m ()
runFollower (Judge run) = run

mkFollower :: LeaderFunction m state
           -> FollowerFunction m state
           -> Follower m state
mkFollower = Follower

debounceLeader :: (Monad m, Eq state) => LeaderFunction m state -> LeaderFunction m state
debounceLeader inner mstate =
    (inner mstate >>= yield) .| debounce
  where
    debounce = await >>= maybe (error "impossible") (\state -> yield state *> loop state)

    loop state1 = await >>= maybe (return state1)
      (\state2 ->
        if state1 == state2
          then loop state1
          else yield state2 >> loop state2)

killableFollower :: (MonadIO m, MonadBaseUnlift IO m)
                 => (state -> m ()) -> FollowerFunction m state
killableFollower inner getState = do
  unlift <- askRunBase
  let loop (stateid, state) =
        join $ withAsync (unlift (inner state)) $ const $ atomically $ do
          (newid, newstate) <- getState
          checkSTM $ stateid /= newid
          return $ loop (newid, newstate)
  liftIO $ atomically getState >>= loop
