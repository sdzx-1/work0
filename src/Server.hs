{-# LANGUAGE TypeApplications #-}

module Server where

import Command
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Data.IORef
import Data.Map as Map
import Manager
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import ServerApi

api :: Proxy ServerApi
api = Proxy

server :: Chan (Client Command) -> TChan (Client Result) -> Server ServerApi
server comChan resultTChan clientid comd =
  liftIO $ do
    writeChan comChan (Client clientid comd)
    atomically $ do
      Client cid result <-
        readTChan
          resultTChan
      if cid == clientid
        then return result
        else retry

app :: Chan (Client Command) -> TChan (Client Result) -> Application
app c r = serve api (server c r)

main :: IO ()
main = do
  gref <- newIORef Map.empty
  comChan <- newChan
  resTChan <- newTChanIO
  forkIO $ managerFrontThread (Manager gref) comChan resTChan
  print "servre start, http port 8081"
  -- writeChan comChan (Client 0 defCommand)
  run 8081 (app comChan resTChan)
