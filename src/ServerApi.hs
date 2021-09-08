{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServerApi where

import Command
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Swagger
import Data.Swagger.Internal.Schema (ToSchema)
import Servant
import Servant.Swagger

type ServerApi = "command" :> "clientid" :> Capture "clientid" Int :> ReqBody '[JSON] Command :> Post '[JSON] Result

instance ToSchema Node

instance ToSchema Graph

instance ToSchema NodeCommand

instance ToSchema GraphCommand

instance ToSchema Command

instance ToSchema Result

mySwagger :: Swagger
mySwagger = toSwagger (Proxy :: Proxy ServerApi)

sgi :: IO ()
sgi = do
  BL.writeFile "api_swagger.json" (encode mySwagger)
