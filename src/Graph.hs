{-# LANGUAGE TemplateHaskell #-}

module Graph where

import B
import Control.Carrier.Lift
import Control.Carrier.State.Strict as S
import Control.Carrier.Store
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Effect.Optics
import Control.Effect.State.Labelled
import Control.Monad
import Control.Monad.IO.Class
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Example
import Data.IORef
import Data.IntMap as IntMap
import qualified Data.List as L
import Data.Map as Map
import Data.Maybe
import Name
import Optics
import System.Directory
import System.Process
import Type

-- >>> tmain tv
-- mkGraph [(0,"s0"),(1,"s1"),(2,"s2"),(3,"s3"),(4,"s4"),(5,"s5"),(6,"s6"),(7,"s7")] [(0,1,1),(1,2,1),(1,3,1),(2,4,1),(2,5,3),(3,5,2),(4,5,1),(5,6,1),(5,7,1)]
tmain :: (Show a, Show b, Graph gr) => gr a b -> IO (gr a b)
tmain a = do
  let graph = a
  let dot = showDot (fglToDot graph)
  writeFile "file.dot" dot
  system "dot -Tpng -o file.png file.dot"
  system "eog file.png"
  return a

g :: Gr String Int
g =
  mkGraph
    [ (0, "s0"),
      (1, "s1"),
      (2, "s2"),
      (3, "s3"),
      (4, "s4"),
      (5, "s5"),
      (6, "s6")
    ]
    [ (0, 1, 1),
      (1, 2, 1),
      (1, 3, 1),
      (3, 5, 2),
      (2, 4, 1),
      (4, 5, 1),
      (5, 6, 1),
      (2, 5, 3)
    ]

-- >>> t
-- mkGraph [(0,"s0"),(1,"s1"),(2,"s2"),(3,"s3"),(4,"s4"),(5,"s5"),(6,"s6"),(7,"s7")] [(0,1,()),(1,2,()),(1,3,()),(2,4,()),(3,5,()),(4,5,()),(5,6,())]
tv =
  let k = topsort' g
      k1 = insEdge (5, 7, 1) $ insNode (7, "s7") g
      k2 = topsort' g
   in k1

type Inputs = [IORef Expr]

type Output = IORef Expr

instance Show (IORef a) where
  show _ = "IORef"

data GlobalState = GlobalState
  { _graph :: Gr String Int,
    _evalList :: [String], -- topsort graph
    _handlersState :: Map String HandlerState
  }
  deriving (Show)

data HandlerState = HanderState
  { _handlerEnv :: Map Name PAddr,
    _handlerStore :: IntMap (Maybe Expr),
    _inputs :: Inputs,
    _output :: Output
  }
  deriving (Show)

makeLenses ''HandlerState
makeLenses ''GlobalState

initGlobalState :: Has (State GlobalState) sig m => Gr String () -> m ()
initGlobalState = undefined

--- insert a node
--- insert a edge of node
--- name
--- Expr  get handler , get args, match args with parent's node, get args --- parens't output IORef
