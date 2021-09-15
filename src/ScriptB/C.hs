module ScriptB.C where

import Eval
import ScriptB.B

test :: IO ()
test = do
  con <- readFile "src/ScriptB/test.txt"
  print con
  case runCalc con of
    Left s -> print s
    Right ex -> do
      print ex
      res <- runEval ex 
      print res
