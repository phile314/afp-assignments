module Afp.As4.ChServer
  ( run )
where

import Afp.As4.Messages
import System.IO
import Network
import Control.Concurrent
import Control.Concurrent.STM

data ChatState = ChatState [Conn]
type ChatState' = TVar ChatState


type Conn = TVar ((Handle, HostName, PortNumber), TChan MsgS2C, Maybe NickName)

run :: IO ()
run = runServer 9595

runServer :: Int -> IO ()
runServer p = withSocketsDo $ do
  s <- listenOn (PortNumber (fromIntegral p))

  cs <- newTVarIO (ChatState [])

  mainLoop cs s
  -- TODO error handling

mainLoop :: ChatState' -> Socket -> IO ()
mainLoop cs s = do
  conn <- accept s

  connInfo <- atomically $ do
    tc  <- newTChan
    tup <- newTVar (conn, tc, Nothing)
    modifyTVar cs (\(ChatState cl) -> (ChatState (tup:cl)))
    return tup

  forkIO $ handleClient cs connInfo conn
  
  mainLoop cs s


handleClient :: ChatState' -> Conn -> (Handle, HostName, PortNumber) -> IO ()
handleClient cs conn (h, _, _) = do
  hSetBuffering h NoBuffering
  handleJoin cs conn h

gMsg :: Handle -> IO MsgC2S
gMsg h = do
  msg <- hGetLine h
  let msg' = read msg :: MsgC2S
  return msg'

sMsg :: Conn -> MsgS2C -> STM ()
sMsg conn msg = do
  (_, tc, _) <- readTVar conn
  writeTChan tc msg
  

handleJoin :: ChatState' -> Conn -> Handle -> IO ()
handleJoin cs co h = do
  (Join nick) <- gMsg h
  putStrLn ("Nick" ++ nick)
  atomically $ do
    (c1, t1, Nothing) <- readTVar co
    writeTVar co (c1, t1, Just nick)

  handleMsgs cs co h


handleMsgs :: ChatState' -> Conn -> Handle -> IO ()
handleMsgs cs co h = do
  m <- gMsg h
  case m of
    (Speak t) -> undefined
    (Leave) -> undefined

