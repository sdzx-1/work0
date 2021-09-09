{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified A
import qualified B
import Test.Hspec
import Type

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented."
  con <- readFile "test/script/objectPair.txt"
  print $ B.runCalc con
  hspec $ do
    describe "parse  " $ do
      it "object" $ do
        B.runCalc con
          `shouldSatisfy` isRight

-- print $ B.runCalc con
