module Main where

import InteractiveGraph
import Control.Monad

main :: IO ()
main = do
  putStrLn "start demo"
  void runGraph