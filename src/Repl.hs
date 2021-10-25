{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Repl where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Store
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Eval
import           ScriptA.B as A
import           ScriptB.B as B
import           Type

runRepl' :: (Has (Env PAddr :+: Error EvalError) sig m,
             HasLabelled Store (Store PAddr Expr) sig m,
             MonadIO m)
         => m ()
runRepl' = do
  input <- liftIO getInput
  v <- catchError @EvalError (evalExpr input) (\e -> liftIO (print e) >> pure (Elit LitNull))
  liftIO $ putStrLn ("..>" ++ show v)
  runRepl'
  where
    getInput :: IO Expr
    getInput = do
      s <- getLine
      case A.runCalc s of
        Left e  -> print e >> getInput
        Right v -> return v

runRepl =
  runM @IO
    . runEnv
    . runStore
    . runError @EvalError
    $ do
      evalExpr (init' (Elit LitNull))
      runRepl'
