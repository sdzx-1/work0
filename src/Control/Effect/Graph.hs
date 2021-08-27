{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Graph where

import Control.Algebra
import Control.Effect.Labelled
import Control.Effect.State.Labelled (HasLabelled)
import Data.Functor.Const
import qualified Data.Graph.Inductive as G
import Data.Kind as K

data Graph a b (m :: K.Type -> K.Type) k where
  IsEmpty :: Graph a b m Bool
  CreateEmpty :: Graph a b m ()
  -- Graph Projection
  Nodes :: Graph a b m [G.Node]
  Edges :: Graph a b m [G.Edge]
  -- Graph Construction and Destruction
  InsNode :: G.LNode a -> Graph a b m ()
  InsEdge :: G.LEdge b -> Graph a b m ()
  InsNodes :: [G.LNode a] -> Graph a b m ()
  InsEdges :: [G.LEdge b] -> Graph a b m ()
  DelNodes :: [G.Node] -> Graph a b m ()
  DelEdges :: [G.Edge] -> Graph a b m ()
  DelNode :: G.Node -> Graph a b m ()
  DelEdge :: G.Edge -> Graph a b m ()
  -- operations
  NodeNumber :: Graph a b m Int
  EdgeNumber :: Graph a b m Int
  -- topsort
  Topsort :: Graph a b m [G.Node]
  -- Graph Inspection
  FindNodeLabel :: G.Node -> Graph a b m (Maybe a)
  Neighboors :: G.Node -> Graph a b m [G.Node]
  Out :: G.Node -> Graph a b m [G.LEdge b]
  Inn :: G.Node -> Graph a b m [G.LEdge b]

isEmpty :: HasLabelled Graph (Graph a b) sig m => m Bool
isEmpty = sendLabelled @Graph IsEmpty

createEmpty :: HasLabelled Graph (Graph a b) sig m => m ()
createEmpty = sendLabelled @Graph CreateEmpty

nodes :: HasLabelled Graph (Graph a b) sig m => m [G.Node]
nodes = sendLabelled @Graph Nodes

edges :: HasLabelled Graph (Graph a b) sig m => m [G.Edge]
edges = sendLabelled @Graph Edges

insNode :: HasLabelled Graph (Graph a b) sig m => G.LNode a -> m ()
insNode g = sendLabelled @Graph (InsNode g)

insEdge :: HasLabelled Graph (Graph a b) sig m => G.LEdge b -> m ()
insEdge g = sendLabelled @Graph (InsEdge g)

insNodes :: HasLabelled Graph (Graph a b) sig m => [G.LNode a] -> m ()
insNodes g = sendLabelled @Graph (InsNodes g)

insEdges :: HasLabelled Graph (Graph a b) sig m => [G.LEdge b] -> m ()
insEdges g = sendLabelled @Graph (InsEdges g)

delNodes :: HasLabelled Graph (Graph a b) sig m => [G.Node] -> m ()
delNodes g = sendLabelled @Graph (DelNodes g)

delEdges :: HasLabelled Graph (Graph a b) sig m => [G.Edge] -> m ()
delEdges g = sendLabelled @Graph (DelEdges g)

delNode :: HasLabelled Graph (Graph a b) sig m => G.Node -> m ()
delNode g = sendLabelled @Graph (DelNode g)

delEdge :: HasLabelled Graph (Graph a b) sig m => G.Edge -> m ()
delEdge g = sendLabelled @Graph (DelEdge g)

nodeNumber :: HasLabelled Graph (Graph a b) sig m => m Int
nodeNumber = sendLabelled @Graph NodeNumber

edgeNumber :: HasLabelled Graph (Graph a b) sig m => m Int
edgeNumber = sendLabelled @Graph EdgeNumber

topsort :: HasLabelled Graph (Graph a b) sig m => m [G.Node]
topsort = sendLabelled @Graph Topsort

findNodeLabel :: HasLabelled Graph (Graph a b) sig m => G.Node -> m (Maybe a)
findNodeLabel g = sendLabelled @Graph (FindNodeLabel g)

neighboors :: HasLabelled Graph (Graph a b) sig m => G.Node -> m [G.Node]
neighboors g = sendLabelled @Graph (Neighboors g)

out :: HasLabelled Graph (Graph a b) sig m => G.Node -> m [G.LEdge b]
out g = sendLabelled @Graph (Out g)

inn :: HasLabelled Graph (Graph a b) sig m => G.Node -> m [G.LEdge b]
inn g = sendLabelled @Graph (Inn g)
