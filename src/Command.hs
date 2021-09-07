{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.Aeson
import GHC.Generics

data Node = Node
  { nodeName :: String,
    nodeId :: Int,
    nodeDescription :: Maybe String,
    nodeInputNodes :: [(Int, Int)],
    nodeScript :: String
  }
  deriving (Generic, FromJSON, ToJSON)

data Graph = Graph
  { graphName :: String,
    graphDescription :: Maybe String,
    graphNodes :: [Node]
  }
  deriving (Generic, FromJSON, ToJSON)

type GraphNumber = Int

type NodeId = Int

type Name = String

data Command
  = CreateGraph Graph
  | RemoveGraph GraphNumber
  | GraphCommend GraphNumber GraphCommand
  | NodeCommand GraphNumber NodeId NodeCommand
  deriving (Generic, FromJSON, ToJSON)

data GraphCommand
  = InsertNode Node
  | RemoveNode NodeId [(NodeId, Int)]
  deriving (Generic, FromJSON, ToJSON)

data NodeCommand
  = LookUpVar String
  | EvalExpr String
  deriving (Generic, FromJSON, ToJSON)