{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Graph where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Trace.Ignoring (Algebra)
import Control.Effect.Graph
import Control.Effect.Labelled
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.PatriciaTree

newtype GraphC a b m v = GraphC {runGraphC :: StateC (Gr a b) m v}
  deriving (Applicative, Functor, Monad)

runGraph :: Applicative m => Labelled Graph (GraphC a b) m v -> m (Gr a b, v)
runGraph = runState G.empty . runGraphC . runLabelled

instance forall sig m a b. Algebra sig m => Algebra (Graph a b :+: sig) (GraphC a b m) where
  alg hdl sig ctx = GraphC $ case sig of
    L IsEmpty -> get @(Gr a b) >>= pure . (<$ ctx) . G.isEmpty
    L CreateEmpty -> put @(Gr a b) (G.empty) >> pure ctx
    L Nodes -> get @(Gr a b) >>= pure . (<$ ctx) . G.nodes
    L Edges -> get @(Gr a b) >>= pure . (<$ ctx) . G.edges
    L (InsNode n) -> modify @(Gr a b) (G.insNode n) >> pure ctx
    L (InsEdge n) -> modify @(Gr a b) (G.insEdge n) >> pure ctx
    L (InsNodes n) -> modify @(Gr a b) (G.insNodes n) >> pure ctx
    L (InsEdges n) -> modify @(Gr a b) (G.insEdges n) >> pure ctx
    L (DelNodes n) -> modify @(Gr a b) (G.delNodes n) >> pure ctx
    L (DelEdges n) -> modify @(Gr a b) (G.delEdges n) >> pure ctx
    L (DelNode n) -> modify @(Gr a b) (G.delNode n) >> pure ctx
    L (DelEdge n) -> modify @(Gr a b) (G.delEdge n) >> pure ctx
    L NodeNumber -> get @(Gr a b) >>= pure . (<$ ctx) . G.order
    L EdgeNumber -> get @(Gr a b) >>= pure . (<$ ctx) . G.size
    L Topsort -> get @(Gr a b) >>= pure . (<$ ctx) . G.topsort
    L (FindNodeLabel n) -> get @(Gr a b) >>= pure . (<$ ctx) . (flip G.lab n)
    L (Neighboors n) -> get @(Gr a b) >>= pure . (<$ ctx) . (flip G.neighbors n)
    L (Out n) -> get @(Gr a b) >>= pure . (<$ ctx) . (flip G.out n)
    L (Inn n) -> get @(Gr a b) >>= pure . (<$ ctx) . (flip G.inn n)
    R other -> alg (runGraphC . hdl) (R other) ctx