{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Graph where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.State.Strict as S
import Control.Carrier.Store hiding ((.=))
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Effect.Optics
import Control.Effect.State.Labelled
import Control.Exception.Base (assert)
import Control.Monad
import Control.Monad.IO.Class
import Control.Tracer
import Data.Graph.Inductive
  ( Gr,
    Graph (mkGraph),
    gelem,
    insEdges,
    insNode,
    prettyPrint,
    topsort,
  )
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Example
import Data.IORef
import Data.IntMap as IntMap
import qualified Data.List as L
import Data.Map as Map
import Data.Maybe
import Data.Typeable
import qualified Eval
import GHC.IOArray (newIOArray)
import Name as N
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

type NodeId = Int

data GraphError
  = GraphInitError NodeId EvalError
  | EvalGraphError NodeId EvalError
  | NodeInputNotExist NodeId NodeId
  | StrangeErrorNodeDeleted NodeId
  | NodeIdExisted NodeId
  | NotDefinedHandler
  | HandlerArgsNotMatch
  | HandlerDefinedTypeError
  deriving (Show)

--- insert a node
--- insert a edge of node
--- name
--- Expr  get handler , get args, match args with parent's node, get args --- parens't output IORef
insertNameNodeEdgeExpr ::
  Has (State GlobalState :+: Error GraphError :+: Lift IO) sig m =>
  String -> -- name
  Expr -> -- expr
  Int -> -- nodeid
  [(Int, Int)] -> -- (sourceNodeid, edge number label as args position)
  m ()
insertNameNodeEdgeExpr name code nodeid edges = do
  -- init code
  let m = Map.empty
      im = IntMap.empty
  -- (a, b, _) <- sendIO $ Eval.runEval' m im (Eval.init' code)
  res <- sendIO (Eval.runEval' m im (Eval.init' code))
  case res of
    Left ee -> throwError (GraphInitError nodeid ee)
    Right (a, b, _) -> do
      -- create HandlerState, env and store ready

      -- create Output
      outputRef <- sendIO $ newIORef defaultExpr

      let rv = do
            PAddr tv <- Map.lookup (N.name "handler") b
            IntMap.lookup tv a
      case join rv of
        Nothing -> throwError NotDefinedHandler
        Just ex ->
          case ex of
            Fun ls _ ->
              when (length ls /= length edges) $
                throwError HandlerArgsNotMatch
            _ -> throwError HandlerDefinedTypeError
      g <- use graph
      when (gelem nodeid g) (throwError (NodeIdExisted nodeid))
      -- create Inputs
      -- 1. insert node
      graph %= insNode (nodeid, name)
      -- 2. insert edges
      graph %= insEdges (fmap (\(a, b) -> (a, nodeid, b)) edges)
      -- 2.1 update evalList
      -- get new graph
      newGraph <- use graph
      sendIO $ prettyPrint newGraph
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
          maybe (throwError $ NodeInputNotExist nodeid sourceNodeid) (return . _output) (Map.lookup sourceNodeid hss)

      -- make HandlerState
      let hs =
            HandlerState
              { _handlerEnv = b,
                _handlerStore = a,
                _inputs = inputs,
                _output = outputRef
              }
      handlersState %= Map.insert nodeid hs

data EvalCommand
  = RunOnce
  | Terminal
  | LookupVar Int Name
  | EvalExpr Int Expr
  | InsertNode String Expr Int [(Int, Int)]
  | LookupGlobalState
  | LookupAllNodes
  | LookupNode Int
  deriving (Show)

data TraceRunGraph
  = TraceCommand EvalCommand
  | TraceResult TraceGraphEval
  deriving (Show)

data EvalResult
  = Successed String
  | Failed String
  deriving (Show)

runGraph :: Has (State GlobalState :+: Error GraphError :+: Lift IO) sig m => Chan EvalCommand -> Tracer m TraceRunGraph -> m ()
runGraph chan tracer =
  sendIO (readChan chan) >>= \case
    Terminal -> do
      s <- S.get @GlobalState
      sendIO $ do
        print "----------------------GlobaseState----------------------"
        print s
        print "--------------------------------------------------------"
    RunOnce -> evalGraph (contramap TraceResult tracer) >> runGraph chan tracer
    _ -> undefined

runGraph' ::
  Has (State GlobalState :+: Error GraphError :+: Lift IO) sig m =>
  MVar EvalCommand ->
  MVar EvalResult ->
  Tracer m TraceRunGraph ->
  m ()
runGraph' mvar rmvar tracer =
  sendIO (tryTakeMVar mvar) >>= \case
    Nothing -> do
      sendIO $ threadDelay (10 ^ 5)
      evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
    Just x -> case x of
      Terminal -> do
        s <- S.get @GlobalState
        sendIO $ do
          print "----------------------GlobaseState----------------------"
          print s
          print "--------------------------------------------------------"
          putMVar rmvar (Successed "terminal successed")
      RunOnce -> evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      LookupVar nodeId name -> do
        hs <- use handlersState
        let res = join $ do
              v <- Map.lookup nodeId hs
              PAddr t1 <- Map.lookup name (v ^. handlerEnv)
              IntMap.lookup t1 (v ^. handlerStore)
        case res of
          Nothing -> sendIO $ putMVar rmvar (Failed $ show (nodeId, name) ++ " var not found ")
          Just v -> sendIO $ putMVar rmvar (Successed $ show v)
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      EvalExpr nodeId e -> do
        hs' <- use handlersState
        case Map.lookup nodeId hs' of
          Nothing -> sendIO $ putMVar rmvar (Failed $ show nodeId ++ " node not exist ")
          Just hs -> do
            res <-
              sendIO $
                Eval.runEval'
                  (hs ^. handlerEnv)
                  (hs ^. handlerStore)
                  e
            case res of
              Left ee -> sendIO $ putMVar rmvar (Failed $ show nodeId ++ " " ++ show ee)
              Right (a, b, _) -> do
                -- update HandlerState
                let newhs = hs {_handlerEnv = b, _handlerStore = a}
                -- update global state
                handlersState %= Map.insert nodeId newhs
                sendIO $ putMVar rmvar (Successed "expr eval successed")
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      InsertNode a b c d -> do
        gs <- S.get @GlobalState
        catchError @GraphError
          (insertNameNodeEdgeExpr a b c d)
          ( \ee -> do
              sendIO $ putMVar rmvar (Failed $ " insert node error: " ++ show ee)
              S.put gs
              evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
          )
        sendIO $ putMVar rmvar (Successed " insert node successed ")
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      LookupGlobalState -> do
        gs <- S.get @GlobalState
        sendIO $ putMVar rmvar (Successed $ show gs)
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      LookupAllNodes -> do
        gs <- S.get @GlobalState
        sendIO $ putMVar rmvar (Successed $ show $ gs ^. graph)
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer
      LookupNode i -> do
        gs <- S.get @GlobalState
        case Map.lookup i (gs ^. handlersState) of 
          Nothing -> sendIO $ putMVar rmvar (Failed "node not exist")
          Just hs -> sendIO $ putMVar rmvar (Successed $ show hs)
        evalGraph (contramap TraceResult tracer) >> runGraph' mvar rmvar tracer

data TraceGraphEval = GR
  { nodeId :: Int,
    vars :: [(Name, Expr)],
    result :: Expr
  }
  deriving (Show, Typeable)

traceFun ::
  Has (State GlobalState :+: Lift IO) sig m =>
  Map Name PAddr ->
  IntMap (Maybe Expr) ->
  Expr ->
  Tracer m TraceGraphEval ->
  Int ->
  m ()
traceFun m im e tracer i = do
  let removeBuildIn Nothing = []
      removeBuildIn (Just e) = [e | not (isBuildIn e)]

      varsVal =
        concatMap
          ( \(name, PAddr i) ->
              fmap (name,) $ removeBuildIn $ join $ IntMap.lookup i im
          )
          (Map.toList m)
  traceWith tracer $
    GR
      { nodeId = i,
        vars = varsVal,
        result = e
      }

evalGraph :: Has (State GlobalState :+: Error GraphError :+: Lift IO) sig m => Tracer m TraceGraphEval -> m ()
evalGraph tracer = do
  elist <- use evalList
  forM_ elist $ \i -> do
    res <- uses handlersState (Map.lookup i)
    case res of
      Nothing -> throwError (StrangeErrorNodeDeleted i)
      Just hs -> do
        -- get all inputs
        inputs <- sendIO $ mapM readIORef (hs ^. inputs)
        if any isSkip inputs
          then do
            traceFun (hs ^. handlerEnv) (hs ^. handlerStore) Skip tracer i
            sendIO $ writeIORef (hs ^. output) Skip
          else do
            -- sendIO $ print $ "------node" ++ show i ++ " start------"
            -- eval node code
            res <-
              sendIO $
                Eval.runEval'
                  (hs ^. handlerEnv)
                  (hs ^. handlerStore)
                  (AppFun (Elit $ LitSymbol "handler") inputs)
            case res of
              Left ee -> throwError (EvalGraphError i ee)
              Right (a, b, c) -> do
                ------------------ trace grap eval result
                traceFun b a c tracer i
                ------------------
                -- write output
                sendIO $ writeIORef (hs ^. output) c
                -- update HandlerState
                let newhs = hs {_handlerEnv = b, _handlerStore = a}
                -- update global state
                handlersState %= Map.insert i newhs

-- sendIO $ print $ "------node" ++ show i ++ " finish-----"
