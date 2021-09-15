{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ScriptB.Stack where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Kind
import ScriptB.A

--- insert to tokens
--LayoutStart LayoutSetp LayoutEnd

-- Layout Stack
-- NewLayoutUninterrupt
-- NewLayout
-- IfLayout
-- ElseLayout

-- alignLayout

-- Layout line column
-- current line

data LayoutClass
  = IfLayout
  | NewLayout
  | NewLayoutUninterrrupt
  deriving (Show, Eq)

-- type Line = Int

-- type Column = Int

data Layout
  = Layout
      { layoutClass :: LayoutClass,
        layoutColumn :: Int
      }
  | CreateNewLayout Int
  | CreateNewLayoutUninterrrupt Int
  deriving (Show, Eq)

newtype LayoutStack = LayoutStack
  { layoutStack :: [Layout]
  }
  deriving (Show)

initLayout :: LayoutStack
initLayout = LayoutStack [CreateNewLayout 0]

initOutput :: Output
initOutput = []

data Line (m :: Type -> Type) a where
  IsNewLine :: Int -> Line m Bool

isNewLine :: Has Line sig m => Int -> m Bool
isNewLine i = send (IsNewLine i)

runLine :: Int -> LineC m a -> m (Int, a)
runLine i = runState i . unLineC

newtype LineC m a = LineC {unLineC :: StateC Int m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (Line :+: sig) (LineC m) where
  alg hdl sig ctx = LineC $ case sig of
    L (IsNewLine i) -> do
      v <- get
      if i > v
        then put i >> pure (True <$ ctx)
        else pure (False <$ ctx)
    R other -> alg (unLineC . hdl) (R other) ctx

type Output = [Token]

data LayoutError
  = LayoutNotMatch
  | NeverHappened
  | IndentError
  | CollapsToIfError
  | LayoutUninterrrupt
  deriving (Show)

getTokenPos :: Token -> Posn
getTokenPos = \case
  String p _ -> p
  Number p _ -> p
  Separators p _ -> p
  KeyWord p _ -> p
  Var p _ -> p
  _ -> error "never happened"

-- searchUp :: Int -> LayoutStack ->

peek :: Has (State LayoutStack) sig m => m (Maybe Layout)
peek = do
  v <- layoutStack <$> get
  case v of
    [] -> return Nothing
    (x : _) -> return (Just x)

pop :: Has (State LayoutStack) sig m => m (Maybe Layout)
pop = do
  v <- layoutStack <$> get
  case v of
    [] -> return Nothing
    (x : xs) -> put (LayoutStack xs) >> return (Just x)

push :: Has (State LayoutStack) sig m => Layout -> m ()
push l = do
  v <- layoutStack <$> get
  put (LayoutStack (l : v))

addToken :: Has (State Output) sig m => Token -> m ()
addToken token = modify (token :)

insertLayout ::
  Has (State LayoutStack :+: State Output :+: Error LayoutError :+: Line) sig m =>
  Token ->
  m ()
insertLayout EOF = collapsAll
insertLayout token = do
  let Posn line column = getTokenPos token
  b <- isNewLine line
  if b
    then do
      peek
        >>= ( \case
                Nothing -> throwError NeverHappened
                Just lay -> case lay of
                  Layout lc n -> do
                    case token of
                      KeyWord po "else" -> collapsToIf column >> addToken token >> push (CreateNewLayout column)
                      _ -> do
                        case compare column n of
                          EQ -> addToken LayoutStep >> addToken token >> isSpecial token
                          GT -> throwError IndentError
                          LT -> collapsToColumn column >> addToken LayoutStep >> addToken token >> isSpecial token
                  CreateNewLayoutUninterrrupt n -> do
                    if column > n
                      then do
                        pop
                        push (Layout NewLayoutUninterrrupt column)
                        addToken LayoutStart
                        addToken token
                        isSpecial token
                      else throwError IndentError
                  CreateNewLayout n ->
                    if column > n
                      then do
                        pop
                        push (Layout NewLayout column)
                        addToken LayoutStart
                        addToken token
                        isSpecial token
                      else throwError IndentError
            )
    else addToken token

collapsAll :: Has (State LayoutStack :+: State Output :+: Error LayoutError) sig m => m ()
collapsAll = do
  pop
    >>= ( \case
            Nothing -> return ()
            Just _ -> addToken LayoutEnd >> collapsAll
        )

collapsToColumn :: Has (State LayoutStack :+: State Output :+: Error LayoutError) sig m => Int -> m ()
collapsToColumn c = do
  v <- layoutStack <$> get
  res <- peek
  case res of
    Nothing -> throwError IndentError
    Just lay -> case lay of
      Layout NewLayoutUninterrrupt column -> if c == column then return () else throwError LayoutUninterrrupt
      Layout _ column ->
        if c == column
          then return ()
          else pop >> addToken LayoutEnd >> collapsToColumn c
      _ -> pop >> addToken LayoutEnd >> collapsToColumn c

collapsToIf :: Has (State LayoutStack :+: State Output :+: Error LayoutError) sig m => Int -> m ()
collapsToIf c = do
  v <- layoutStack <$> get
  res <- pop
  case res of
    Nothing -> throwError CollapsToIfError
    Just lay -> case lay of
      Layout IfLayout column -> if c == column then return () else throwError CollapsToIfError
      _ -> addToken LayoutEnd >> collapsToIf c

isSpecial ::
  Has (State LayoutStack :+: State Output :+: Error LayoutError :+: Line) sig m =>
  Token ->
  m ()
isSpecial = \case
  KeyWord (Posn _ c) s -> case s of
    "def" -> push (CreateNewLayout c)
    "while" -> push (CreateNewLayout c)
    "if" -> push (Layout IfLayout c) >> push (CreateNewLayoutUninterrrupt c)
    _ -> return ()
  _ -> return ()

runLayout inputs =
  run $
    runState initLayout $
      runState initOutput $
        runLine 0 $
          runError @LayoutError $
            mapM insertLayout inputs
