module ScriptB.Stack where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
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

type Line = Int

type Column = Int

data Layout = Layout LayoutClass Line Column

newtype LayoutStack = LayoutStack [Layout]

type Input = [Token]

type Output = [Token]