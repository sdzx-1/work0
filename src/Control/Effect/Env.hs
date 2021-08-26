{-# LANGUAGE GADTs #-}

module Control.Effect.Env
  ( -- * Env effect
    -- bind,
    binds,
    varDec,
    lookupEnv,
    Env (..),

    -- * Re-exports
    Algebra,
    Has,
    run,
  )
where

import Control.Algebra
import Name

-- bind :: Has (Env addr) sig m => Name -> addr -> m a -> m a
-- bind name addr m = send (Bind name addr m)

lookupEnv :: Has (Env addr) sig m => Name -> m (Maybe addr)
lookupEnv name = send (Lookup name)

binds :: Has (Env addr) sig m => [(Name, addr)] -> m a -> m a
binds bs m = send (Binds bs m)

varDec :: Has (Env addr) sig m => Name -> addr -> m ()
varDec name addr = send (VarDec name addr)

data Env addr m k where
--   Bind :: Name -> addr -> m a -> Env addr m a
  Lookup :: Name -> Env addr m (Maybe addr)
  ---
  Binds :: [(Name, addr)] -> m a -> Env addr m a
  VarDec :: Name -> addr -> Env addr m ()
