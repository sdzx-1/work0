{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module T1 where

import Control.Algebra
import Control.Carrier.Error.Church (Catch (Catch), Throw (Throw))
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad
import Data.Array.IO
import Data.Array.MArray
import Data.Functor.Identity
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Show (Show)
import Name
import Type

{-

build in function
  + - * /  > >= < <= !=

global var

local functoin

local var

----------------------
Expr
----------------------

var x;
var x = 5;
var y = 10

for (var i; i < (x+y) ; i++) {
    print(i);
}

stack small big
array

[]-[i]-[]-[]-[]

function f1(a,b) {
    return(a+b);
}

 -}

-- ts :: Has (State [(Int, Int)] :+: Reader (IOArray Int)) sig m => m ()
-- ts = undefined

{-

var chain  [Map String Expr]

 -}

{-

build in function
  + - * /  > >= < <= !=

global var

local functoin

local var

----------------------
Expr
----------------------

var x;
var x = 5;
var y = 10

for (var i=0; i < (x+y) ; i++) {
    print(i);
}

stack small big
array

[]-[i]-[]-[]-[]

function f1(a,b) {
    return(a+b);
}

 -}

tf :: IO ()
tf = do
  ls <- forM [1 .. 10] $ \i -> do
    ref <- newIORef i
    return (i, ref)
  v <- readIORef $ snd $ head ls
  print v

evalExpr :: Has (State (Map Name Expr) :+: Error EvalError :+: Lift IO) sig m => Expr -> m Expr
evalExpr (Elit e) = evalLit e
evalExpr (Var name e) = modify @(Map Name Expr) (M.insert name e) >> return e
evalExpr (Exprs ls) = last <$> mapM evalExpr ls
evalExpr (Fun args e) = evalExpr e
evalExpr (AppFun e args) = do
  -- e         function or function ref
  m <- get @(Map Name Expr)
  case e of
    --- e as function
    (Fun dargs de) -> do
      if length dargs /= length args
        then throwError ArgsNotMatch
        else do
          args' <- mapM evalExpr args
          modify @(Map Name Expr) (M.union (M.fromList (zip dargs args')))
          evalExpr de
    --- e as function ref
    (Elit (LitSymbol s)) -> do
      case M.lookup s m of
        Nothing -> throwError SymbolNotFind
        Just (Fun dargs de) -> do
          if length dargs /= length args
            then throwError ArgsNotMatch
            else do
              args' <- mapM evalExpr args -- eval args before function call
              modify @(Map Name Expr) (M.union (M.fromList (zip dargs args')))
              evalExpr de
        Just (BuildInFunction f) -> do
          args' <- mapM evalExpr args
          v <- sendIO $ f args'
          either throwError return v
        _ -> throwError RefTypeError
    _ -> throwError AppExprError
-- put m -- restore state
-- return r
evalExpr (Return e) = evalExpr e
evalExpr (For e1 e2 e3 e4) = do
  m <- get @(Map Name Expr)
  evalExpr e1
  ne2 <- evalExpr e2
  let go (Elit (LitBool True)) val = do
        ne3 <- evalExpr e3
        v4 <- evalExpr e4
        ne2 <- evalExpr e2
        -- sendIO $ print (ne3, v4, ne2)
        go ne2 v4
      go _ val = return val

  go ne2 (Elit LitNull)
-- put m -- restore state
-- return res
evalExpr (Assignment name e) = do
  e' <- evalExpr e
  modify @(Map Name Expr) (M.insert name e')
  return e'
evalExpr _ = throwError NeverHappened

evalLit :: Has (State (Map Name Expr) :+: Error EvalError :+: Lift IO) sig m => Lit -> m Expr
evalLit (LitSymbol sym) = do
  m <- get @(Map Name Expr)
  maybe (throwError SymbolNotFind) return (M.lookup sym m)
evalLit (LitObject ls) = do
  v <- forM ls $ \(name, e) -> do
    e' <- evalExpr e
    return (name, e)
  return (Elit $ LitObject v)
evalLit (LitArray ls) = Elit . LitArray <$> mapM evalExpr ls
evalLit e = return (Elit e)

add :: [Expr] -> IO (Either EvalError Expr)
add [Elit (LitNum b1), Elit (LitNum b2)] = return $ Right $ Elit $ LitNum (b1 + b2)
add ls = return $ Left $ AddTypeError ls

less :: [Expr] -> IO (Either EvalError Expr)
less [Elit (LitNum b1), Elit (LitNum b2)] = return $ Right $ Elit $ LitBool (b1 < b2)
less ls = return $ Left $ LessTypeError ls

logger :: [Expr] -> IO (Either EvalError Expr)
logger ls = print ls >> return (Right (head ls))

runEval :: Expr -> IO (Map Name Expr, Either EvalError Expr)
runEval e =
  runM @IO $
    runState @(Map Name Expr)
      ( M.fromList
          [ ("+", BuildInFunction add),
            ("<", BuildInFunction less),
            ("logger", BuildInFunction logger)
          ]
      )
      $ runError @EvalError $ evalExpr e

-- >>> runEval t
-- (fromList [(Name "+", build in function),(Name "<", build in function),(Name "a", lit: LitNum 1.0),(Name "a1", lit: LitNum 10.0),(Name "add", fun: [Name "a1",Name "b1"] exprs: [ AppFun:  lit: LitSymbol (Name "logger")[ lit: LitSymbol (Name "c")], return:  AppFun:  lit: LitSymbol (Name "+")[ lit: LitSymbol (Name "a1"), lit: LitSymbol (Name "b1")]]),(Name "b", lit: LitNum 2.0),(Name "b1", lit: LitNum 45.0),(Name "c", lit: LitNum 55.0),(Name "i", lit: LitNum 10.0),(Name "logger", build in function)],Right  lit: LitNum 55.0)
