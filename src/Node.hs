{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad
import Control.Tracer
import GUI
import Graph
import Widget

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

  (r, f, m, pe, ge) <- initGUI

  -- fork graph worker
  forkIO $
    void $
      runM $
        runState @GlobalState initGlobalState $ do
          -- init
          forM_ ls $ \(a, b, c, d) -> insertNameNodeEdgeExpr a b c d
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
        if i == 100
          then writeChan chan Terminal
          else do
            threadDelay (10 ^ 6)
            go (i + 1)
  --- send command
  forkIO $ go 0

  runReader (UIEnv r f m ge) $ runState makeUIState appLoop1
  threadDelay (10 ^ 6)