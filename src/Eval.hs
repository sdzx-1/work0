{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Eval where

import B
import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.State.Strict as S
import Control.Carrier.Store
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Effect.State.Labelled
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap as IntMap
import qualified Data.List as L
import Data.Map as Map
import Data.Maybe
import Name
import System.Directory
import System.Random
import Type

evaLit ::
  (Has (Env PAddr :+: Error EvalError) sig m, HasLabelled Store (Store PAddr Expr) sig m, MonadIO m) => Lit -> m Expr
evaLit = \case
  LitSymbol n -> do
    a <- lookupEnv n
    -- sendIO $ print (n, a)
    maybe (pure $ Elit $ LitSymbol n) fetch a
  LitObject ls -> do
    ls' <- forM ls $ \(name, e) -> do
      e' <- evalExpr e
      return (name, e')
    pure $ Elit $ LitObject ls'
  LitArray arr -> Elit . LitArray <$> mapM evalExpr arr
  other -> pure $ Elit other

evalExpr ::
  (Has (Env PAddr :+: Error EvalError) sig m, HasLabelled Store (Store PAddr Expr) sig m, MonadIO m) => Expr -> m Expr
evalExpr = \case
  Exprs ls -> last <$> mapM evalExpr ls
  For e1 e2 e3 e4 -> do
    binds @PAddr [] $ do
      evalExpr e1
      let go val = do
            evalExpr e2 >>= \case
              Elit (LitBool True) -> do
                v3 <- evalExpr e3
                v4 <- evalExpr e4
                go v4
              _ -> return val
      go (Elit LitNull)
  IfElse a b c -> do
    evalExpr a >>= \case
      Elit (LitBool True) -> evalExpr b
      Elit (LitBool False) -> evalExpr c
      _ -> return (Elit LitNull)
  Return e -> evalExpr e
  Var name v -> do
    a <- alloc name
    v' <- evalExpr v
    a .= v'
    varDec name a
    return v'
  Elit lit -> evaLit lit
  Fun names v -> pure (Fun names v)
  AppFun v args -> do
    evalExpr v >>= \case
      Fun names1 e -> do
        when (length names1 /= length args) (throwError ArgsNotMatch)
        eargs <- mapM evalExpr args
        addrs <- forM (zip names1 eargs) $ \(n, e) -> do
          n' <- alloc n
          n' .= e
          pure n'
        binds (zip names1 addrs) (evalExpr e)
      BuildInFunction f -> do
        eargs <- mapM evalExpr args
        liftIO (f eargs) >>= \case
          Left e -> throwError e
          Right v -> return v
      o -> throwError $ UnSpportAppFun (show (AppFun v args))
  Assignment name e -> do
    a <- lookupEnv name
    e' <- evalExpr e
    maybe (throwError $ VarNotDefined (show name)) (.= e') a
    return e'
  BuildInFunction f -> return (BuildInFunction f)
  Skip -> return Skip
  ObjectGet name ls -> do
    evalExpr (Elit $ LitSymbol name) >>= \case
      p@(Elit (LitObject pairs)) -> case getFold ls p of
        Left ee -> throwError ee
        Right ex -> return ex
      _ -> throwError (NotObject name)

getFold :: [Name] -> Expr -> Either EvalError Expr
getFold [] e = Right e
getFold (a : ls) (Elit (LitObject pairs)) =
  case Prelude.lookup a pairs of
    Nothing -> Left (ObjectNotF a)
    Just ex -> getFold ls ex
getFold (a : _) _ = Left (ObjectNotF a)

runEval :: Expr -> IO (Either EvalError (Map Name PAddr, (PStore Expr, Expr)))
runEval expr =
  runError @EvalError
    . runEnv
    . runStore
    $ evalExpr (init' expr)

add :: [Expr] -> IO (Either EvalError Expr)
add [Elit (LitNum b1), Elit (LitNum b2)] = return $ Right $ Elit $ LitNum (b1 + b2)
add ls = return $ Left $ AddTypeError ls

less :: [Expr] -> IO (Either EvalError Expr)
less [Elit (LitNum b1), Elit (LitNum b2)] = return $ Right $ Elit $ LitBool (b1 < b2)
less ls = return $ Left $ LessTypeError ls

logger :: [Expr] -> IO (Either EvalError Expr)
logger ls = print ls >> return (Right (head ls))

random' :: [Expr] -> IO (Either EvalError Expr)
random' [Elit (LitNum b1), Elit (LitNum b2)] = do
  v <- randomRIO (b1, b2)
  return $ Right $ Elit $ LitNum v
random' ls = return $ Left $ LessTypeError ls

skip' :: [Expr] -> IO (Either EvalError Expr)
skip' _ = return (Right Skip)

-- >>> runEval t
--  lit: LitSymbol (Name "<")
init' :: Expr -> Expr
init' e =
  Exprs $
    [ Var "+" $ BuildInFunction add,
      Var "<" $ BuildInFunction less,
      Var "logger" $ BuildInFunction logger,
      Var "random" $ BuildInFunction random',
      Var "skip" $ BuildInFunction skip'
    ]
      ++ [e]

runEval' ::
  Map Name PAddr ->
  IntMap (Maybe Expr) ->
  Expr ->
  IO
    ( Either
        EvalError
        (IntMap (Maybe Expr), Map Name PAddr, Expr)
    )
runEval' env store expr = do
  ( runError @EvalError
      . runEnv' env
      . runStore' store
      $ evalExpr expr
    )
    >>= ( \case
            Left se -> return $ Left se
            Right (b, (PStore a, c)) -> do
              let ls = Map.toList b
                  t = Prelude.map (\(name, PAddr i) -> (i, join $ IntMap.lookup i a)) ls
                  nim = IntMap.fromList t
              return $ Right (nim, b, c)
        )

-- runEval :: Expr -> IO
-- test
test = do
  con <- readFile "test/script/objectPair.txt"
  case runCalc con of
    Left s -> print s
    Right e -> do
      res <- runEval e
      print res