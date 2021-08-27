{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module InteractiveGraph where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.Graph
import Control.Carrier.Lift
import Control.Effect.Graph
import Control.Effect.Labelled
import Control.Effect.State.Labelled (HasLabelled)
import Control.Monad
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive.Dot
import Graph (tmain)
import System.Process
import Text.Read

data ErrorType
  = Finish
  | ArgsParseError String
  | E2
  deriving (Show)

parse :: (Has (Error ErrorType) sig m, Read a) => String -> m a
parse a = maybe (throwError $ ArgsParseError a) return $ readMaybe a

eval' ::
  ( Has (Lift IO :+: Error ErrorType) sig m,
    HasLabelled Graph (Graph Int Int) sig m,
    Has (Fresh) sig m
  ) =>
  String ->
  m ()
eval' l = do
  case words l of
    "show" : _ -> getGraph >>= sendIO . void . tmain
    "empty" : _ -> createEmpty >> eval' "show"
    "isEmpty" : _ -> isEmpty >>= sendIO . print
    "nodes" : _ -> nodes >>= sendIO . print
    "edges" : _ -> edges >>= sendIO . print
    ["+a", i] -> parse i >>= flip replicateM (fresh >>= \i -> insNode (i, i)) >> eval' "show"
    "+a" : _ -> fresh >>= \i -> insNode (i, i) >> eval' "show"
    ["+e", s, t] -> (,,1) <$> parse s <*> parse t >>= insEdge >> eval' "show"
    "+e" : s : t : m : _ -> (,,) <$> parse s <*> parse t <*> parse m >>= insEdge >> eval' "show"
    "finish" : _ -> throwError Finish
    _ -> sendIO $ print "unsport command"

run' ::
  ( Has (Lift IO :+: Error ErrorType) sig m,
    HasLabelled Graph (Graph Int Int) sig m,
    Has (Fresh) sig m
  ) =>
  m ()
run' = do
  l <- sendIO getLine
  catchError @ErrorType
    (eval' l)
    ( \case
        ArgsParseError s -> sendIO $ print $ "ArgsParseError: " ++ s
        e -> throwError e
    )
  run'

runGraph :: IO (Gr Int Int, (Int, Either ErrorType ()))
runGraph =
  runM @IO $
    Control.Carrier.Graph.runGraph $
      runFresh 0 $ runError @ErrorType run'