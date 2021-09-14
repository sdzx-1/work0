{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ScriptB.Stack where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
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
  | ElseLayout
  | NewLayout
  | NewLayoutUninterrrupt

-- type Line = Int

-- type Column = Int

data Layout = Layout LayoutClass Int Int

newtype LayoutState = LayoutState
  { layoutStack :: [Layout]
  }

initLayout :: LayoutState
initLayout = LayoutState []

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

type Input = [Token]

type Output = [Token]

data LayoutError = LayoutError

insertLayout :: Has (State LayoutState :+: State Output :+: Error LayoutError :+: Line) sig m => Input -> m ()
insertLayout = undefined

runLayout input =
  run $
    runState initLayout $
      runState @Output [] $
        runLine 0 $
          runError @LayoutError $
            insertLayout input