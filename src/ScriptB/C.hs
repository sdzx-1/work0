module ScriptB.C where

import ScriptB.B 

test :: IO ()
test = do
  con <- readFile "src/ScriptB/test.txt"
  print con
  case runCalc con of 
    Left s -> print s
    Right ex -> print ex
