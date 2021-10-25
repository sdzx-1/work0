{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module InteractiveGraph where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Graph
import           Control.Carrier.Lift
import           Control.Effect.Graph
import           Control.Effect.Labelled
import           Control.Effect.State.Labelled (HasLabelled)
import           Control.Monad
import           Data.Either
import           Data.Graph.Inductive (Gr)
import           Data.Graph.Inductive.Dot
import qualified Data.List as L
import           Graph (tmain)
import           System.Process
import           Text.Read

data ErrorType
  = Finish
  | ArgsParseError String
  | E2
  deriving (Show)

parse :: (Has (Error ErrorType) sig m,
          Read a)
      => String
      -> m a
parse a = either (throwError . ArgsParseError . ((a ++ " ") ++)) return $ readEither a

data Node a = Node
  { nodeId       :: Int,
    sourceNodeId :: [Int],
    code         :: a
  }
  deriving (Show)

helpCode :: [Int] -> String
helpCode ls =
  let k = case ls of
        [] -> "input"
        _  -> L.intercalate "," $ map (("node" ++) . show) ls
   in "function handler(" ++ k ++ "){\n \n \n}"

eval' :: (Has (Lift IO :+: Error ErrorType) sig m,
         HasLabelled Graph (Graph Int Int) sig m,
         Has Fresh sig m)
      => String
      -> m ()
eval' l = do
  case words l of
    "show" : _ -> getGraph >>= sendIO . void . tmain
    "empty" : _ -> createEmpty >> eval' "show"
    "isEmpty" : _ -> isEmpty >>= sendIO . print
    "nodes" : _ -> nodes >>= sendIO . print
    "edges" : _ -> edges >>= sendIO . print
    n : "<-" : ls -> do
      nodeId <- parse n
      insNode (nodeId, nodeId)
      ls' <- mapM parse ls
      forM_ (zip [1 ..] ls') $ \(i, s) -> do
        insEdge (s, nodeId, i)
      eval' "show"
      sendIO $ do
        writeFile ("work/m0/node" ++ show nodeId ++ ".txt") (helpCode ls')
        void $ system ("gedit work/m0/node" ++ show nodeId ++ ".txt")
    ["+a", i] -> parse i >>= flip replicateM (fresh >>= \i -> insNode (i, i)) >> eval' "show"
    "+a" : _ -> fresh >>= \i -> insNode (i, i) >> eval' "show"
    ["+e", s, t] -> (,,1) <$> parse s <*> parse t >>= insEdge >> eval' "show"
    [s, "->", t, m] -> (,,) <$> parse s <*> parse t <*> parse m >>= insEdge >> eval' "show"
    [s, "->", t] -> (,,1) <$> parse s <*> parse t >>= insEdge >> eval' "show"
    "+e" : s : t : m : _ -> (,,) <$> parse s <*> parse t <*> parse m >>= insEdge >> eval' "show"
    "file" : _ -> sendIO $ system "gedit work/ww.txt" >> print "edit finish"
    "finish" : _ -> throwError Finish
    _ -> sendIO $ print "unsport command"

run' :: (Has (Lift IO :+: Error ErrorType) sig m,
        HasLabelled Graph (Graph Int Int) sig m,
        Has Fresh sig m)
     => m ()
run' = do
  l <- sendIO getLine
  catchError @ErrorType
    (eval' l)
    ( \case
        ArgsParseError s -> sendIO $ print $ "ArgsParseError: " ++ s
        e                -> throwError e
    )
  run'

runGraph :: IO (Gr Int Int, (Int, Either ErrorType ()))
runGraph =
  runM @IO $
    Control.Carrier.Graph.runGraph $
      runFresh 0 $ runError @ErrorType run'
