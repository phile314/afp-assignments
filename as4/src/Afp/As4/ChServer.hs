{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Afp.As4.ChServer
  ( runServer, runServer' )
where

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

type ConnS1 = Conn String C2SMsg
type ConnS2 = Conn String String

type NickName = String

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

  forkIO $ withConn hi (\c -> handleExs cs c $ handleJoin cs) False

  mainLoop cs s

handleExs :: ChatState' -> ConnS1 -> (ConnS2 -> IO ()) -> IO ()
handleExs cs conn p = do
    let f = (p conn' `E.finally` delConn) `E.catch` errH
      in E.catchJust (\e -> if isEOFError e then Just e else Nothing) f (\e -> return ())
    where
        errH :: ExitException -> IO ()
        errH _ = return ()
        conn' :: ConnS2
        conn' = conn { cread =
            E.onException (do
                msg <- cread conn
                case msg of
                    Exit    -> E.throw ExitException
                    (S str) -> return str
                ) (cclose conn')}

        delConn = do
                    nick <- atomically $ do
                        nick <- readTVar $ nickName conn
                        when (isJust nick) $ modifyTVar cs (\m -> ChatState { clients = delete (fromJust nick) (clients m) })
                        return nick
                    when (isJust nick) $ broadcast cs ("* " ++ (fromJust nick) ++ " left.") (fromJust nick)


handleJoin :: ChatState' -> ConnS2 -> IO ()
handleJoin cs conn = do
    nick <- cread conn
    case nick of
        [] -> cwrite conn (errMsg "Nick too short.") >> handleJoin cs conn
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
                Left _  -> cwrite conn (errMsg "Nick already in use.")     >> handleJoin cs conn
                Right _ -> broadcast cs ("* " ++ nick ++ " joined.") nick  >> handleMsgs cs conn

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

handleMsgs :: ChatState' -> ConnS2 -> IO ()
handleMsgs cs conn = do
    m <- cread conn
    nick <- atomically $ do
        nick <- readTVar $ nickName conn
        return $ fromJust nick
    broadcast cs ("[" ++ nick ++ "] " ++ m) nick
    handleMsgs cs conn 


