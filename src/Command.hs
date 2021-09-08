{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy as BSL

type GraphId = Int

type NodeId = Int

type Name = String

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

-- >>> BSL.writeFile "test.json" (encode defCommand)
defCommand =
  CreateGraph
    ( Graph
        { graphName = "test"
        , graphDescription = Just "something"
        , graphNodes = [
          Node {
            nodeName = "Source",
            nodeId = 0 ,
            nodeDescription = Nothing ,
            nodeInputNodes = [],
            nodeScript = "function handler(){ logger(1); return(1) }"

          }
        ]
        }
    )

data Command
  = CreateGraph
      {createGraph :: Graph}
  | RemoveGraph
      {removeGarphId :: GraphId}
  | GraphCommand
      { graphId :: GraphId,
        graphCommand :: GraphCommand
      }
  | NodeCommand
      { graphId :: GraphId,
        nodeId :: NodeId,
        nodeCommand :: NodeCommand
      }
  deriving (Generic, FromJSON, ToJSON)

data GraphCommand
  = InsertNode {insertNode :: Node}
  | RemoveNode
      { nodeId :: NodeId,
        dependNodeSource :: [(NodeId, Int)]
      }
  deriving (Generic, FromJSON, ToJSON)

data NodeCommand
  = LookUpVar String
  | EvalExpr String
  deriving (Generic, FromJSON, ToJSON)

data Result
  = Success String
  | Failed String
  deriving (Generic, FromJSON, ToJSON)

type Id = Int

data Client a = Client Id a
