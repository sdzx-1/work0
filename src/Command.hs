{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.Aeson
import Data.ByteString.Lazy as BSL
import GHC.Generics

type GraphId = Int

type NodeId = Int

type Name = String

newtype Script = Script
  { script :: String
  }
  deriving (Generic, FromJSON, ToJSON)

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

data Command
  = CreateGraph
      {createGraph :: Graph} -- POST /api/creategraph (json Graph)
  | RemoveGraph
      {removeGarphId :: GraphId} -- DELETE /api/graphid/1
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
  = InsertNode {insertNode :: Node} -- POST /api/graphid/1/insertnode (json Node)
  | RemoveNode
      { nodeId :: NodeId,
        dependNodeSource :: [(NodeId, Int)]
      }
  deriving (Generic, FromJSON, ToJSON)

data NodeCommand
  = LookUpVar String -- GET   /api/graphid/1/nodeid/0/getvar/a
  | EvalExpr String -- POST  /api/graphid/1/nodeid/0/evalexpr  (json scriptString)
  deriving (Generic, FromJSON, ToJSON)

data Result
  = Success String
  | Failed String
  deriving (Generic, FromJSON, ToJSON)

type Id = Int

data Client a = Client Id a

-- >>> BSL.writeFile "careteGraph.json" (encode defCreateGraph)
defCreateGraph =
  CreateGraph
    ( Graph
        { graphName = "test",
          graphDescription = Just "something",
          graphNodes =
            [ Node
                { nodeName = "Source",
                  nodeId = 0,
                  nodeDescription = Nothing,
                  nodeInputNodes = [],
                  nodeScript = "var a = 0; function handler(){ a = a + 1; logger(a); return(a) }"
                }
            ]
        }
    )

defRemoveGraph = RemoveGraph 1

-- >>> BSL.writeFile "work/insertNode.json" (encode defInsertNode)
defNodeCommand =
  NodeCommand
    { graphId = 1,
      nodeId = 0,
      nodeCommand = LookUpVar "a"
    }

defNodeCommand1 =
  NodeCommand
    { graphId = 1,
      nodeId = 0,
      nodeCommand = EvalExpr "a = 10"
    }

defInsertNode =
  GraphCommand
    { graphId = 1,
      graphCommand =
        InsertNode $
          Node
            { nodeName = "Source",
              nodeId = 1,
              nodeDescription = Nothing,
              nodeInputNodes = [(0, 1)],
              nodeScript = " function handler(a){ a = a + 1; logger(a); return(a) }"
            }
    }
