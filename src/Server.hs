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

api1 :: Proxy Api
api1 = Proxy

server1 :: Chan (Client Command) -> TChan (Client Result) -> Server Api
server1 comChan resultTChan =
  handler1
    :<|> handler2
    :<|> handler3
    :<|> handler4
    :<|> handler5
  where
    fun comd =
      liftIO $ do
        writeChan comChan (Client 0 comd)
        atomically $ do
          Client cid result <-
            readTChan
              resultTChan
          if cid == 0
            then return result
            else retry

    handler1 g = fun (CreateGraph g)
    handler2 i = fun (RemoveGraph i)
    handler3 i n = fun (GraphCommand i (InsertNode n))
    handler4 i n s = fun (NodeCommand i n (LookUpVar s))
    handler5 i n (Script s) = fun (NodeCommand i n (EvalExpr s))

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
app c r = serve api1 (server1 c r)

main :: IO ()
main = do
  gref <- newIORef Map.empty
  counref <- newIORef 0
  comChan <- newChan
  resTChan <- newTChanIO
  forkIO $ managerFrontThread (Manager gref counref) comChan resTChan
  print "servre start, http port 8081"
  -- writeChan comChan (Client 0 defCommand)
  run 8081 (app comChan resTChan)
