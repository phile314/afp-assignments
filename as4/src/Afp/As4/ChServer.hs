module Afp.As4.ChServer
  ( runServer, runServer' )
where

import Afp.As4.Messages
import System.IO
import System.IO.Error
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Maybe
import Data.Map
import Afp.As4.ChConn

data ChatState = ChatState
  { clients :: Map NickName ConnS }
type ChatState' = TVar ChatState

type ConnS = Conn MsgS2C MsgC2S


runServer' :: IO ()
runServer' = runServer 9595

runServer :: Int -> IO ()
runServer p = withSocketsDo $ do
  s <- listenOn (PortNumber (fromIntegral p))

  cs <- newTVarIO (ChatState empty)

  E.finally (mainLoop cs s) (sClose s)

mainLoop :: ChatState' -> Socket -> IO ()
mainLoop cs s = do
  hi@(h,_, _) <- accept s

  forkIO $ withConn hi (\c -> handleExs cs c $ handleJoin cs c)

  mainLoop cs s

handleExs :: ChatState' -> ConnS -> IO () -> IO ()
handleExs cs conn p = do
    p `E.catch` hErr
    where
        hErr :: IOError -> IO ()
        hErr e = do
                    nick <- atomically $ do
                        nick <- readTVar $ nickName conn
                        when (isJust nick) $ modifyTVar cs (\m -> ChatState { clients = delete (fromJust nick) (clients m) })
                        return nick
                    when (isJust nick) $ broadcast' cs (SLeft $ fromJust nick)


handleJoin :: ChatState' -> ConnS -> IO ()
handleJoin cs conn = do
  m <- cread conn
  case m of
        (CJoin nick) -> do
            cs' <- atomically $ do
                modifyTVar cs (\(ChatState cl) -> (ChatState (insert nick conn cl)))
                writeTVar (nickName conn) (Just nick)
                readTVar cs
            broadcast cs' (SJoined nick)
            handleMsgs cs conn
        _ -> cwrite conn errMsg >> handleJoin cs conn

broadcast :: ChatState -> MsgS2C -> IO ()
broadcast cs msg = mapM_ (\c -> cwrite c msg) (elems $ clients cs)
broadcast' :: ChatState' -> MsgS2C -> IO ()
broadcast' cs msg = (atomically $ readTVar cs) >>= (\cs' -> broadcast cs' msg)

handleMsgs :: ChatState' -> ConnS -> IO ()
handleMsgs cs conn = do
    m <- cread conn
    case m of
        (CSpeak t) -> do
            nick <- atomically $ readTVar $ nickName conn
            broadcast' cs (SSpoke (fromJust nick) t)
            handleMsgs cs conn 
        _ -> cwrite conn errMsg


