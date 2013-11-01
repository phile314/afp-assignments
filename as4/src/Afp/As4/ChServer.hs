{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A chat server.
module Afp.As4.ChServer
  ( runServer, main )
where

import System.Environment
import Debug.Trace
import System.IO
import System.IO.Error
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Maybe
import Data.Map hiding (filter, map)
import Afp.As4.ChConn
import Data.Typeable

data ExitException = ExitException
    deriving (Show, Typeable)

instance E.Exception ExitException

data ChatState = ChatState
  { clients :: Map NickName ConnS2 }
type ChatState' = TVar ChatState

-- We need to wrap the message to transport the exit notification.
-- We could also encode them using a magic string, but to do this
-- proper this would require us to have some form of string escaping.
-- With an extra type, we can just leverage the show/read functions.
type ConnS1 = Conn String C2SMsg
type ConnS2 = Conn String String

type NickName = String


-- | Run the server.
main :: IO ()
main = runServer 9595

-- | Run the server.
runServer :: Int -> IO ()
runServer p = withSocketsDo $ do
  s <- listenOn (PortNumber (fromIntegral p))

  cs <- newTVarIO (ChatState empty)

  E.finally (mainLoop cs s) (sClose s)

-- | Main loop - accept connections and start threads to handle them.
mainLoop :: ChatState' -> Socket -> IO ()
mainLoop cs s = do
  hi@(h,_, _) <- accept s

  forkIO $ withConn hi (\c -> handleExs cs c $ handleJoin cs) False

  mainLoop cs s

-- | Handles exceptions / closing of connections. Removes closed connections
--   from the global state.
handleExs :: ChatState' -> ConnS1 -> (ConnS2 -> IO ()) -> IO ()
handleExs cs conn p = do
    let f = (p conn' `E.finally` delConn) `E.catch` errH
      in E.catchJust (\e -> if isEOFError e then Just e else Nothing) f (\e -> return ())
    where
        errH :: ExitException -> IO ()
        errH _ = return ()
        -- handle Exit messages here and never return them back, instead throw an exception to leave the read loop
        conn' :: ConnS2
        conn' = conn { cread =
            E.onException (do
                msg <- cread conn
                case msg of
                    Exit    -> E.throw ExitException
                    (S str) -> return str
                ) (cclose conn')}

        -- client disappeared, remove it from global state and broadcast notification
        delConn = do
                    nick <- atomically $ do
                        nick <- readTVar $ nickName conn
                        when (isJust nick) $ modifyTVar cs (\m -> ChatState { clients = delete (fromJust nick) (clients m) })
                        return nick
                    when (isJust nick) $ broadcast cs ("* " ++ (fromJust nick) ++ " left.") (fromJust nick)


-- | Handle joining a room and validiting nick names. Goes to handleMsgs when successful.
--   (this is kind of like continuation passing, but we always have the same continuation)
handleJoin :: ChatState' -> ConnS2 -> IO ()
handleJoin cs conn = do
    nick <- cread conn
    case nick of
        [] -> cwrite conn (errMsg "Nick too short, enter another nick.") >> handleJoin cs conn
        _  -> do
            cs' <- atomically $ do
                cs' <- readTVar cs
                if nick `member` (clients cs') then do
                    return $ Left ()
                else do
                    writeTVar cs (cs' {clients = insert nick conn (clients cs')})
                    writeTVar (nickName conn) (Just nick)
                    return $ Right ()
            case cs' of
                Left _  -> cwrite conn (errMsg "Nick already in use, enter another nick.")     >> handleJoin cs conn
                Right _ -> broadcast cs ("* " ++ nick ++ " joined.") nick  >> handleMsgs cs conn

-- | Send a message to all clients, except to the sender.
broadcast :: ChatState' -> String -> NickName -> IO ()
broadcast cs msg snd = do
    rec <- atomically $ do
        cs' <- readTVar cs
        cs' <- filterM (\c -> do
            n <- readTVar $ nickName c
            return $ (fromJust n) /= snd
            ) $ elems $ clients cs'
        return cs'
    mapM_ (\c -> cwrite c msg) rec

-- | Broadcast all incoming messages to all other clients.
handleMsgs :: ChatState' -> ConnS2 -> IO ()
handleMsgs cs conn = do
    m <- cread conn
    nick <- atomically $ do
        nick <- readTVar $ nickName conn
        return $ fromJust nick
    broadcast cs ("[" ++ nick ++ "] " ++ m) nick
    handleMsgs cs conn 


