{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Node where

import B
import qualified Command as C
import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Concurrent
import Control.Effect.Optics
import Control.Exception
import Control.Monad
import Control.Tracer
import GUI
import Graph
import System.Environment
import Text.Read
import Widget
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

data Node = Node String Int [(Int, Int)] deriving (Read, Show)

start :: IO ()
start = do
  workDir : _ <- getArgs

  chan <- newChan

  g <- readFile $ workDir ++ "/g.txt"
  print g
  let ns = Prelude.map (read @Node) (lines g)

  ls <- forM ns $ \(Node s a b) -> do
    ne' <- runCalc <$> readFile (workDir ++ "/" ++ s ++ ".txt")
    tcon <- readFile (workDir ++ "/" ++ s ++ ".txt")
    ne <- case ne' of
      Left e -> error $ show $ "module is " ++ s ++ " " ++ show e
      Right v -> return v
    let cnode = C.Node s a Nothing b tcon
    return ((s, ne, a, b), cnode)
  -- print ls
  let cGraph = C.Graph "example" Nothing (map snd ls)
  BSL.writeFile (workDir ++ "/" ++ "graph" ++ ".json") (encode cGraph)
  (r, f, m, pe, ge) <- initGUI

  -- fork graph worker
  forkIO $
    void $
      runError @GraphError $
        runState @GlobalState initGlobalState $ do
          -- init
          forM_ ls $ \((a, b, c, d), _) -> insertNameNodeEdgeExpr a b c d
          g <- use graph
          sendIO $ forkIO $ void $ tmain g
          let tracer :: Has (State GlobalState :+: Lift IO) sig m => TraceRunGraph -> m ()
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