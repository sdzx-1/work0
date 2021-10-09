{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Store
  ( -- * Store carrier
    PAddr (..),
    PStore (..),
    PEnv,
    runStore,
    runStore',
    StoreC (StoreC),

    -- * Store effect
    module Control.Effect.Store,

    -- * Env carrier
    runEnv,
    runEnv',
    EnvC (..),
    InternalError (..),

    -- * Env effect
    module Control.Effect.Env,
  )
where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Church
import Control.Effect.Env
import Control.Effect.Error
import Control.Effect.Labelled
import Control.Effect.Store
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Data.IntMap as IntMap
import Data.Map as Map
import Name

newtype PAddr = PAddr Int deriving (Show)

newtype PStore a = PStore (IntMap.IntMap (Maybe a))
  deriving (Eq, Ord, Monoid, Semigroup, Show)

type PEnv = Map.Map Name PAddr

-- Store carrier

runStore :: Monad m => Labelled Store (StoreC val) m a -> m (Either InternalError (PStore val, a))
runStore = runError @InternalError . runState (curry pure) mempty . runStoreC . runLabelled

runStore' :: Monad m => IntMap (Maybe val) -> Labelled Store (StoreC val) m a -> m (Either InternalError (PStore val, a))
runStore' im = runError @InternalError . runState (curry pure) (PStore im) . runStoreC . runLabelled

data InternalError
  = InternalErrorUnallocatedAddr
  | InternalErrorUninitializedAddr
  deriving (Show)

newtype StoreC val m a = StoreC {runStoreC :: StateC (PStore val) (ErrorC InternalError m) a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra (Store PAddr val :+: sig) (StoreC val m) where
  alg hdl sig ctx = StoreC $ case sig of
    L op -> case op of
      Alloc _ -> StateC $ \k (PStore heap) -> do
        let a = maybe 0 ((+ 1) . fst) (IntMap.lookupMax heap)
        k (PStore (IntMap.insert a Nothing heap)) (PAddr a <$ ctx)
      Assign (PAddr a) v -> ctx <$ modify (\(PStore heap) -> PStore (IntMap.insert a (Just v) heap))
      Fetch (PAddr a) -> StateC $ \k (PStore heap) -> do
        case IntMap.lookup a heap of
          Nothing -> throwError InternalErrorUnallocatedAddr
          (Just Nothing) -> throwError InternalErrorUninitializedAddr
          (Just (Just v)) -> k (PStore heap) (v <$ ctx)
    R other -> alg (runStoreC . hdl) (R (R other)) ctx

-- Env carrier

runEnv :: Applicative m => EnvC m a -> m (Map Name PAddr, a)
runEnv = runState (curry pure) Map.empty . runEnvC

runEnv' :: Applicative m => Map Name PAddr -> EnvC m a -> m (Map Name PAddr, a)
runEnv' m = runState (curry pure) m . runEnvC

newtype EnvC m a = EnvC {runEnvC :: StateC PEnv m a}
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadIO)

instance Algebra sig m => Algebra (Env PAddr :+: sig) (EnvC m) where
  alg hdl sig ctx = EnvC $ case sig of
    -- L (Bind name addr m) -> local (Map.insert name addr) (runEnvC (hdl (m <$ ctx)))
    L (Lookup name) -> gets ((<$ ctx) . Map.lookup name)
    --------
    L (Binds ls m) -> do
      m' <- get @PEnv
      modify (Map.union (Map.fromList ls))
      v <- runEnvC (hdl (m <$ ctx))
      put m'
      pure v
    L (VarDec name addr) -> do
      modify (Map.insert name addr)
      pure ctx
    R other -> alg (runEnvC . hdl) (R other) ctx
