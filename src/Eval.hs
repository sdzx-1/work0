{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Eval where

import B
import Control.Carrier.Lift
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
  (Has (Env PAddr) sig m, HasLabelled Store (Store PAddr Expr) sig m, Has (Lift IO) sig m) => Lit -> m Expr
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
  (Has (Env PAddr) sig m, HasLabelled Store (Store PAddr Expr) sig m, Has (Lift IO) sig m) => Expr -> m Expr
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
        when (length names1 /= length args) (error (show args))
        eargs <- mapM evalExpr args
        addrs <- forM (zip names1 eargs) $ \(n, e) -> do
          n' <- alloc n
          n' .= e
          pure n'
        binds (zip names1 addrs) (evalExpr e)
      BuildInFunction f -> do
        eargs <- mapM evalExpr args
        sendIO (f eargs) >>= \case
          Left e -> error (show e)
          Right v -> return v
      o -> error (show (AppFun v args))
  Assignment name e -> do
    a <- lookupEnv name
    e' <- evalExpr e
    maybe (error (show name)) (.= e') a
    return e'
  BuildInFunction f -> return (BuildInFunction f)
  Skip -> return Skip

runEval :: Expr -> IO (PStore Expr, (Map Name PAddr, Expr))
runEval expr =
  runM @IO
    . runStore
    . runEnv
    $ evalExpr expr

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

-- >>> runEval t
--  lit: LitSymbol (Name "<")
init' :: Expr -> Expr
init' e =
  Exprs $
    [ Var "+" $ BuildInFunction add,
      Var "<" $ BuildInFunction less,
      Var "logger" $ BuildInFunction logger,
      Var "random" $ BuildInFunction random'
    ]
      ++ [e]

runEval' env store expr = do
  (PStore a, (b, c)) <-
    runM @IO
      . runStore' store
      . runEnv' env
      $ evalExpr expr
  let ls = Map.toList b
      t = Prelude.map (\(name, PAddr i) -> (i, join $ IntMap.lookup i a)) ls
      nim = IntMap.fromList t
  return (nim, b, c)

type GlobalState = Map String (Map Name PAddr, IntMap (Maybe Expr))

data Command
  = Push Expr
  | PushByName String Expr
  | Terminal
  deriving (Show)

tmain = do
  undefined

workflow :: (Has (State GlobalState) sig m, Has (Lift IO) sig m) => [(String, Expr)] -> Chan Command -> m ()
workflow codes chan = do
  forM_ codes $ \(name, code) -> do
    let m = Map.empty
        im = IntMap.empty
    --- init
    (a, b, _) <- sendIO $ runEval' m im (init' code)
    S.modify @GlobalState $ Map.insert name (b, a)

  m <- S.get @GlobalState
  let names = Prelude.map fst codes
  go names chan
  where
    go :: (Has (State GlobalState) sig m, Has (Lift IO) sig m) => [String] -> Chan Command -> m ()
    go names chan = do
      sendIO (readChan chan) >>= \case
        Push e -> do
          foldM_
            ( \b name -> do
                (m, im) <- fromMaybe (error "never") <$> S.gets @GlobalState (Map.lookup name)
                sendIO $ print $ "eval node: " ++ name
                (a, b, c) <- sendIO $ runEval' m im (AppFun (Elit $ LitSymbol "handler") [b])
                S.modify (Map.insert name (b, a))
                return c
            )
            e
            names
          go names chan
        PushByName name e -> do
          (m, im) <- fromMaybe (error "never") <$> S.gets @GlobalState (Map.lookup name)
          (a, b, c) <- sendIO $ runEval' m im e
          S.modify (Map.insert name (b, a))
          go names chan
        Terminal -> do
          s <- S.get @GlobalState
          sendIO $ print s

k = do
  dis <- listDirectory "work"
  let names = Prelude.map ("work/" ++) $ L.sort dis
  -- let names = ["work/s.txt", "work/s1.txt"]
  es <- Prelude.map runCalc <$> mapM readFile names
  chan <- newChan
  forkIO $ void $ runM $ runState @GlobalState Map.empty $ workflow (zip names es) chan

  let go i = do
        print "write chan"
        writeChan chan (Push (Elit (LitNum 40)))
        when (i `mod` 5 == 0) (writeChan chan (PushByName "work/s1.txt" (Assignment (Name "count") (Elit (LitNum 0)))))
        if i == 20
          then writeChan chan Terminal
          else do
            threadDelay (10 ^ 6)
            go (i + 1)
  go 0
