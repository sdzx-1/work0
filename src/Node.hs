{-# LANGUAGE TypeApplications #-}

module Node where

import B
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Concurrent
import Control.Effect.Optics
import Control.Monad
import Control.Tracer
import Graph

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