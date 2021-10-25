{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ServerApi where

import           Command
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Swagger
import           Data.Swagger.Internal.Schema (ToSchema)
import           Servant
import           Servant.Swagger

type ServerApi = "command" :> "clientid" :> Capture "clientid" Int :> ReqBody '[JSON] Command :> Post '[JSON] Result

type Api =
  "api" :> "graphs" :> ReqBody '[JSON] Graph :> Post '[JSON] Result
    :<|> "api" :> "graphs" :> Get '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> Get '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> "nodes" :> Get '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> "nodes" :> Capture "node id" Int :> Get '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> Delete '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> ReqBody '[JSON] Node :> Post '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> "nodes" :> Capture "node id" Int :> "getvar" :> Capture "var name" String :> Get '[JSON] Result
    :<|> "api" :> "graphs" :> Capture "graph id" Int :> "nodes" :> Capture "node id" Int :> "evalexpr" :> ReqBody '[JSON] Script :> Post '[JSON] Result

instance ToSchema Node

instance ToSchema Graph

instance ToSchema Result

instance ToSchema Script

mySwagger :: Swagger
mySwagger = toSwagger (Proxy :: Proxy Api)

sgi :: IO ()
sgi = do
  BL.writeFile "api_swagger.json" (encode mySwagger)
