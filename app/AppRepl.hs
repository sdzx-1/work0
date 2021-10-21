module Main where

import Repl
import System.Environment

main :: IO ()
main = do
  putStrLn "start repl"
  runRepl
  putStrLn "stop repl"
