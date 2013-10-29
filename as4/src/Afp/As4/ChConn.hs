{-# LANGUAGE DeriveDataTypeable  #-}
module Afp.As4.ChConn
  ( Conn (..)
  , withConn)
where

import Afp.As4.Messages
import System.IO
import System.IO.Error
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable
import Control.Exception (finally, catchJust, Exception(..), throw)
import Data.Maybe
import Data.Map


data Conn a b = Conn
  { cwrite :: a -> IO ()
  , cread :: IO b
  , cclose :: IO ()
  , handle :: Handle
  , hostName :: HostName
  , portNumber :: PortNumber
  , nickName :: TVar (Maybe NickName) }



withConn :: (Msg a, Msg b) => (Handle, HostName, PortNumber) -> (Conn a b -> IO ()) -> IO ()
withConn (h, hn, pn) cont = do

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
      , nickName = tnn }

    return (tw, c)

  forkIO $ writer tw conn
  cont conn
  


writer :: Msg a => TChan (Maybe a) -> Conn a b -> IO ()
writer tw conn = do
    msg <- atomically $ readTChan tw
    case msg of
        (Just msg') -> do
                        hPutStrLn (handle conn) (toStr msg')
                        writer tw conn
        Nothing -> return ()

cread' :: (Msg b, Msg a) => Conn a b -> IO b
cread' conn = do
    finally (do
            msg <- hGetLine (handle conn)
            case fromStr msg of
                (Just msg') -> return msg'
                Nothing     -> (cwrite conn) errMsg >> cread' conn)
        (cclose conn)


cclose' conn tw = do
    atomically $ do
        writeTChan tw Nothing
    hClose (handle conn)
