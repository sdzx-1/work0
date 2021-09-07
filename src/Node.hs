{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Node where

import B
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Concurrent
import Control.Effect.Optics
import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Aeson
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import GHC.Generics
import GHC.Generics (Generic)
import GUI
import Graph
import System.Environment
import Text.Read
import Type (Expr)
import Widget

data Node1 = Node1
  { nodeName :: String,
    nodeId :: Int,
    nodeDescription :: Maybe String,
    nodeInputNodes :: [(Int, Int)],
    nodeScript :: String
  }
  deriving (Generic, FromJSON, ToJSON)

data Graph1 = Graph1
  { graphName :: String,
    graphDescription :: Maybe String,
    graphNodes :: [Node1]
  }
  deriving (Generic, FromJSON, ToJSON)

data Node = Node String Int [(Int, Int)] deriving (Read, Show)

start :: IO ()
start = do
  workDir : _ <- getArgs

  chan <- newChan

  g <- readFile $ workDir ++ "/g.txt"
  print g
  let ns = Prelude.map (read @Node) (lines g)

  ls <- forM ns $ \(Node s a b) -> do
    ne' <- try @SomeException $ runCalc <$> readFile (workDir ++ "/" ++ s ++ ".txt")
    ne <- case ne' of
      Left e -> error $ show $ "module is " ++ s ++ " " ++ show e
      Right v -> return v
    -------------------------------------
    nodeCon <- readFile (workDir ++ "/" ++ s ++ ".txt")
    let node1 =
          Node1
            { nodeName = s,
              Node.nodeId = a,
              nodeDescription = Just $ s ++ " some description",
              nodeInputNodes = b,
              nodeScript = nodeCon
            }
    -- writeFile (workDir ++ "/" ++ s ++ ".json") undefined
    -- BSL.writeFile (workDir ++ "/" ++ s ++ ".json") (encode node1)
    -------------------------------------
    print ne
    return ((s, ne, a, b), node1)
  -- print ls
  let graphv = Graph1 "example0" (Just "example0 description") (map snd ls)
  BSL.writeFile (workDir ++ "/dag.json") (encode graphv)
  (r, f, m, pe, ge) <- initGUI

  -- fork graph worker
  forkIO $
    void $
      runM $
        runState @GlobalState initGlobalState $ do
          -- init
          forM_ ls $ \((a, b, c, d), _) -> insertNameNodeEdgeExpr a b c d
          g <- use graph
          sendIO $ forkIO $ void $ tmain g
          let tracer :: Has (State GlobalState Control.Algebra.:+: Lift IO) sig m => TraceRunGraph -> m ()
              tracer va = do
                case va of
                  TraceCommand c -> sendIO $ print c
                  TraceResult v -> sendIO $ void $ pe v
          runGraph chan (Tracer tracer)

  let go i = do
        print "write chan"
        writeChan chan RunOnce
        if i == 10000
          then writeChan chan Terminal
          else do
            threadDelay (10 ^ 6)
            go (i + 1)
  --- send command
  forkIO $ go 0

  pos' <- try @SomeException $ readFile $ workDir ++ "/position.txt"
  let poss = case pos' of
        Left _ -> makeBP $ length ns
        Right s -> case readMaybe s of
          Nothing -> makeBP $ length ns
          Just a -> a
  runReader (UIEnv r f m ge) $ runState (makeUIState workDir poss) appLoop1
  threadDelay (10 ^ 6)

type NodeId = Int

type NodeState = String
-- instance FromJSON Expr 
-- instance ToJSON Expr

data Command
  = EvalDag Graph1
  | InsertNode Node1
  | RemoveNode Int
  | EvalExpr Int Expr
  -- deriving (Generic, FromJSON, ToJSON)

data EvalState = Success | Failed 

data Result 
  = ExprEvalState 
  | Error NodeId String
  | EvalResult  NodeState
