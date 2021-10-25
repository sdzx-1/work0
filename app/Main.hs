module Main where

import qualified Node as N
import qualified GUI as G
import qualified Server as S

main :: IO ()
main = do
  putStrLn "start demo"
  -- start
  -- G.main
  N.start
  -- S.main
