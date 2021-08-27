{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Graph where

import B
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.State.Strict as S
import Control.Carrier.Store hiding ((.=))
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Effect.Optics
import Control.Effect.State.Labelled
import Control.Exception.Base (assert)
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
import qualified Eval
import GHC.IOArray (newIOArray)
import Name
import Optics (makeLenses, (^.))
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
      k2 = topsort g
   in k1

type Inputs = [IORef Expr]

type Output = IORef Expr

instance Show (IORef a) where
  show _ = "IORef"

data GlobalState = GlobalState
  { _graph :: Gr String Int,
    _evalList :: [Int], -- topsort graph
    _handlersState :: Map Int HandlerState
  }
  deriving (Show)

data HandlerState = HandlerState
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

defaultExpr = Elit (LitNum 10)

--- insert a node
--- insert a edge of node
--- name
--- Expr  get handler , get args, match args with parent's node, get args --- parens't output IORef
insertNameNodeEdgeExpr ::
  Has (State GlobalState :+: Lift IO) sig m =>
  String -> -- name
  Expr -> -- expr
  Int -> -- nodeid
  [(Int, Int)] -> -- (sourceNodeid, edge number label as args position)
  m ()
insertNameNodeEdgeExpr name code nodeid edges = do
  -- init code
  let m = Map.empty
      im = IntMap.empty
  (a, b, _) <- sendIO $ Eval.runEval' m im (Eval.init' code)
  -- create HandlerState, env and store ready

  -- create Output
  outputRef <- sendIO $ newIORef defaultExpr

  -- TODO: check handler args match to egdes's length

  -- create Inputs
  -- 1. insert node
  graph %= insNode (nodeid, name)
  -- 2. insert edges
  graph %= insEdges (fmap (\(a, b) -> (a, nodeid, b)) edges)
  -- 2.1 update evalList
  -- get new graph
  newGraph <- use graph
  -- update evalList
  evalList .= topsort newGraph
  -- 3. finds all source output IORef as Inputs
  -- 3.1 sort edges by args position  (5,2) (7,3) (9,1) -> (9,1) (5,2) (7,3)
  let edges' = L.sortBy (\(_, a) (_, b) -> compare a b) edges
  --   assert  (5,2) (5,3) (5,1) -> (5,1) (5,2) (5,3)
  sendIO $ print $ assert (and $ zipWith (==) [1 ..] (fmap snd edges')) "assert success"
  --  3.2 find all source nodeid IORef
  inputs <- forM edges' $ \(sourceNodeid, _) -> do
    hss <- use handlersState
    maybe (error "nodeid Not find") (return . _output) (Map.lookup sourceNodeid hss)

  -- make HandlerState
  let hs =
        HandlerState
          { _handlerEnv = b,
            _handlerStore = a,
            _inputs = inputs,
            _output = outputRef
          }
  handlersState %= Map.insert nodeid hs

data Command
  = Push Expr
  | Terminal
  deriving (Show)

runGraph :: Has (State GlobalState :+: Lift IO) sig m => Chan Command -> m ()
runGraph chan =
  sendIO (readChan chan) >>= \case
    Terminal -> do
      s <- S.get @GlobalState
      sendIO $ do
        print "----------------------GlobaseState----------------------"
        print s
        print "--------------------------------------------------------"
    Push e -> evalGraph e >> runGraph chan

evalGraph :: Has (State GlobalState :+: Lift IO) sig m => Expr -> m ()
evalGraph e = do
  elist <- use evalList
  uses handlersState (_inputs . fromMaybe (error "null list") . Map.lookup (head elist)) >>= \case
    [input] -> sendIO $ writeIORef input e
    _ -> error "not support m source"
  evalGraph'

evalGraph' :: Has (State GlobalState :+: Lift IO) sig m => m ()
evalGraph' = do
  elist <- use evalList
  forM_ elist $ \i -> do
    sendIO $ print $ "------node" ++ show i ++ " start------"
    hs <- uses handlersState (fromMaybe (error "node not fing") . Map.lookup i)
    -- get all inputs
    inputs <- sendIO $ mapM readIORef (hs ^. inputs)
    -- eval node code
    (a, b, c) <-
      sendIO $
        Eval.runEval'
          (hs ^. handlerEnv)
          (hs ^. handlerStore)
          (AppFun (Elit $ LitSymbol "handler") inputs)
    -- write output
    sendIO $ writeIORef (hs ^. output) c
    -- update HandlerState
    let newhs = hs {_handlerEnv = b, _handlerStore = a}
    -- update global state
    handlersState %= Map.insert i newhs
    sendIO $ print $ "------node" ++ show i ++ " finish-----"