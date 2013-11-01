{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
module Afp.As4.ChConn
  ( Conn (..)
  , withConn
  , errMsg
  , toStr
  , fromStr
  , C2SMsg (..) )
where

import Safe
import System.IO
import System.IO.Error
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally, catchJust, Exception(..), throw, bracket, onException)

import Data.Maybe
import Data.Map


type NickName = String


class Msg a where
    toStr :: a -> String
    fromStr :: String -> Maybe a
    errMsg :: String -> a

data C2SMsg
  = S String
  | Exit
  deriving (Show, Read, Eq)

instance Msg C2SMsg where
    toStr = show
    fromStr = readMay
    errMsg str = S $ errMsg str

instance Msg String where
    toStr = id
    fromStr = Just
    errMsg str = "ERROR: " ++ str

-- | Represents a connection.
data Conn a b = Conn
  { cwrite :: a -> IO ()
  , cread :: IO b
  , cclose :: IO ()
  , handle :: Handle
  , hostName :: HostName
  , portNumber :: PortNumber
  , nickName :: TVar (Maybe NickName)
  , debug :: Bool }


-- | Initializes the connection, executes the given continuation and closes the connection afterwards.
withConn :: (Msg a, Msg b) => (Handle, HostName, PortNumber) -> (Conn a b -> IO ()) -> Bool -> IO ()
withConn (h, hn, pn) cont dbg = do

    hSetBuffering h NoBuffering
    hSetEncoding h utf8

    (tw, conn) <- atomically $ do
        tw <- newTChan
        tnn <- newTVar Nothing

        let c = Conn {
            cwrite = \m -> atomically $ writeTChan tw (Just m)
          , cread  = cread' c
          , cclose = cclose' c tw
          , handle = h
          , hostName = hn
          , portNumber = pn
          , nickName = tnn
          , debug = dbg }
        return (tw, c)

    finally
      (do
        forkIO $ writer tw conn
        cont conn)
      (do
        when dbg $ putStrLn "Closing..."
        cclose conn)
  

-- | Reads messages from the given channel and writes them to the socket.
--   This ensures that all messages are written correctly onto the socket,
--   as it is not specified in the documentation to which extent writing
--   to sockets is thread-safe.
writer :: Msg a => TChan (Maybe a) -> Conn a b -> IO ()
writer tw conn = do
    msg <- atomically $ readTChan tw
    case msg of
        (Just msg') -> do
                        when (debug conn) $ putStrLn $ "OUT: " ++ (toStr msg')
                        hPutStrLn (handle conn) (toStr msg')
                        writer tw conn
        Nothing -> return ()

-- | Reads messages from the socket. If an invalid message is encountered,
--   an error message is sent back until a valid message arrives.
cread' :: (Msg b, Msg a) => Conn a b -> IO b
cread' conn = do
    onException (do
        msg <- hGetLine (handle conn)
        when (debug conn) $ putStrLn $ "IN: " ++ msg
        case (fromStr msg) of
            (Just msg') -> return msg'
            Nothing     -> (cwrite conn) (errMsg "Invalid message.") >> cread' conn
        ) errH
    where
        errH :: IO ()
        errH = do
            cclose conn

-- | Closes the connection and releases all resources.
cclose' conn tw = do
    atomically $ do
        writeTChan tw Nothing
    
    hClose (handle conn)


