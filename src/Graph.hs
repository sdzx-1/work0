{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Tracer
import Data.Graph.Inductive
  ( Gr,
    Graph (mkGraph),
    insEdges,
    insNode,
    topsort,
  )
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

tmain :: (Show a, Show b, Graph gr) => gr a b -> IO (gr a b)
tmain a = do
  let graph = a
  let dot = showDot (fglToDot graph)
  writeFile "file.dot" dot
  system "dot -Tpng -o file.png file.dot"
  system "eog file.png"
  return a

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

initGlobalState :: GlobalState
initGlobalState =
  GlobalState
    { _graph = mkGraph [] [],
      _evalList = [],
      _handlersState = Map.empty
    }

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
  inputs <-
    forM edges' $ \(sourceNodeid, _) -> do
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
  = RunOnce
  | Terminal
  deriving (Show)

data TraceRunGraph
  = TraceCommand Command
  | TraceResult TraceGraphEval
  deriving (Show)

runGraph :: Has (State GlobalState :+: Lift IO) sig m => Chan Command -> Tracer m TraceRunGraph -> m ()
runGraph chan tracer =
  sendIO (readChan chan) >>= \case
    Terminal -> do
      s <- S.get @GlobalState
      sendIO $ do
        print "----------------------GlobaseState----------------------"
        print s
        print "--------------------------------------------------------"
    RunOnce -> evalGraph (contramap TraceResult tracer) >> runGraph chan tracer

data TraceGraphEval = GR
  { nodeId :: Int,
    vars :: [(Name, Expr)],
    result :: Expr
  }
  deriving (Show)

evalGraph :: Has (State GlobalState :+: Lift IO) sig m => Tracer m TraceGraphEval -> m ()
evalGraph tracer = do
  elist <- use evalList
  forM_ elist $ \i -> do
    hs <- uses handlersState (fromMaybe (error "node not fing") . Map.lookup i)
    -- get all inputs
    inputs <- sendIO $ mapM readIORef (hs ^. inputs)
    if any isSkip inputs
      then sendIO $ writeIORef (hs ^. output) Skip
      else do
        sendIO $ print $ "------node" ++ show i ++ " start------"
        -- eval node code
        (a, b, c) <-
          sendIO $
            Eval.runEval'
              (hs ^. handlerEnv)
              (hs ^. handlerStore)
              (AppFun (Elit $ LitSymbol "handler") inputs)
        ------------------ trace grap eval result
        let removeBuildIn Nothing = []
            removeBuildIn (Just e) = [e | not (isBuildIn e), not (isFun e)]

            varsVal =
              concatMap
                ( \(name, PAddr i) ->
                    fmap (name,) $ removeBuildIn $ join $ IntMap.lookup i a
                )
                (Map.toList b)
        traceWith tracer $
          GR
            { nodeId = i,
              vars = varsVal,
              result = c
            }
        ------------------
        -- write output
        sendIO $ writeIORef (hs ^. output) c
        -- update HandlerState
        let newhs = hs {_handlerEnv = b, _handlerStore = a}
        -- update global state
        handlersState %= Map.insert i newhs

-- sendIO $ print $ "------node" ++ show i ++ " finish-----"

data Node = Node String Int [(Int, Int)] deriving (Read, Show)

start :: IO ()
start = do
  chan <- newChan

  g <- readFile "work/g.txt"
  print g
  let ns = Prelude.map (read @Node) (lines g)

  ls <- forM ns $ \(Node s a b) -> do
    ne <- runCalc <$> readFile ("work/" ++ s ++ ".txt")
    return (s, ne, a, b)

  -- fork graph worker
  forkIO $
    void $
      runM $
        runState @GlobalState initGlobalState $ do
          -- init
          forM_ ls $ \(a, b, c, d) -> insertNameNodeEdgeExpr a b c d
          g <- use graph
          sendIO $ forkIO $ void $ tmain g
          runGraph chan (contramap show stdoutTracer)

  let go i = do
        print "write chan"
        writeChan chan RunOnce
        if i == 100
          then writeChan chan Terminal
          else do
            threadDelay (10 ^ 6)
            go (i + 1)
  go 0
  threadDelay (10 ^ 6)
