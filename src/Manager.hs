{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Manager where

import B (runCalc)
import Command
import Control.Algebra ((:+:))
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Effect.Optics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Tracer
import Data.IORef
import Data.Map as Map
import Data.Text (pack)
import qualified Graph as G
import Name
import Optics

--- main thread

--- servant thread

--- manager thread
---   manager front thread    -- create graph
---   manager backend thread  -- catch error or finish

--- graph thread
newtype RunResult = RunResult String deriving (Show)

newtype Manager = Manager
  {_graphs :: IORef (Map GraphId (MVar G.EvalCommand, MVar G.EvalResult, MVar RunResult))}

makeLenses ''Manager

managerFrontThread ::
  Manager ->
  Chan (Client Command) ->
  TChan (Client Result) ->
  IO ()
managerFrontThread manager command result = do
  Client clientId commd <- readChan command
  case commd of
    CreateGraph gr -> do
      -- gid <- fresh
      -- TODO: 1.check graph 2.check arg match
      -- init graph
      -- expr parser
      let ls = forM (graphNodes gr) $ \(Node a b _ c d) -> do
            case runCalc d of
              Left s -> Left $ "nodeName: " ++ a ++ " nodeId: " ++ show b ++ " " ++ s
              Right ex -> Right (a, ex, b, c)
      case ls of
        Left s -> awtc result clientId (Failed s)
        Right x0 -> do
          -- init node
          res <-
            runError @G.GraphError $
              runState @G.GlobalState G.initGlobalState $
                do
                  forM_ x0 $ \(a, b, c, d) -> G.insertNameNodeEdgeExpr a b c d
          case res of
            Left ge -> awtc result clientId (Failed (show ge))
            Right (x1, ()) -> do
              evalCommand <- newEmptyMVar
              evalResult <- newEmptyMVar
              runResult <- newEmptyMVar
              let fun =
                    runError @G.GraphError $
                      runState @G.GlobalState x1 (G.runGraph' evalCommand evalResult nullTracer)
              -- fork graph thread
              void $ forkIO $ graphThread runResult fun
              let mref = manager ^. graphs
              mmap <- readIORef mref
              let size = Map.size mmap
              modifyIORef mref (Map.insert (size + 1) (evalCommand, evalResult, runResult))
              awtc result clientId (Success $ "graph thread forked, graphId is  " ++ show (size + 1))
      managerFrontThread manager command result
    RemoveGraph n -> do
      let mref = manager ^. graphs
      mmap <- readIORef mref
      case Map.lookup n mmap of
        Nothing -> awtc result clientId (Failed $ "graph " ++ show n ++ " not exist")
        Just (a, b, _) -> do
          putMVar a G.Terminal
          res <- takeMVar b
          modifyIORef mref (Map.delete n)
          awtc result clientId (Success (show res))
      managerFrontThread manager command result
    GraphCommand n gc -> do
      let mref = manager ^. graphs
      mmap <- readIORef mref
      case Map.lookup n mmap of
        Nothing -> awtc result clientId (Failed $ "graph " ++ show n ++ " not exist")
        Just (a, b, c) -> do
          runState <- tryTakeMVar c
          case runState of
            -- graph thread terminate , maybe error happened
            Just rr -> awtc result clientId (Failed $ show rr)
            Nothing -> do
              case gc of
                RemoveNode i x0 -> undefined
                InsertNode no@Node {..} -> do
                  case runCalc nodeScript of
                    Left s -> awtc result clientId (Failed s)
                    Right ex -> do
                      putMVar a (G.InsertNode nodeName ex nodeId nodeInputNodes)
                      res <- takeMVar b
                      awtc result clientId (Success $ show res)
      managerFrontThread manager command result
    NodeCommand n i nc -> do
      let mref = manager ^. graphs
      mmap <- readIORef mref
      case Map.lookup n mmap of
        Nothing -> awtc result clientId (Failed $ "graph " ++ show n ++ " not exist")
        Just (a, b, c) -> do
          runState <- tryTakeMVar c
          case runState of
            -- graph thread terminate , maybe error happened
            Just rr -> awtc result clientId (Failed $ show rr)
            Nothing -> do
              case nc of
                LookUpVar s -> do
                  putMVar a (G.LookupVar i (name $ pack s))
                  res <- takeMVar b
                  awtc result clientId (Success $ " lookup var success: " ++ show res)
                EvalExpr s -> do
                  case runCalc s of
                    Left str -> awtc result clientId (Failed $ "expr parse error: " ++ str)
                    Right ex -> do
                      putMVar a (G.EvalExpr i ex)
                      res <- takeMVar b
                      awtc result clientId (Success $ "success: " ++ show res)
      managerFrontThread manager command result

awtc :: TChan (Client Result) -> Id -> Result -> IO ()
awtc tc i res = atomically (writeTChan tc (Client i res))

graphThread :: MVar RunResult -> IO (Either G.GraphError (G.GlobalState, ())) -> IO ()
graphThread result fun = do
  res <- fun
  case res of
    Left ge -> putMVar result (RunResult (show ge))
    Right (x0, _) -> putMVar result (RunResult "global State")