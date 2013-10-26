module Afp.As4.ChConn
  ( Conn (..)
  , withConn )
where

import Afp.As4.Messages
import System.IO
import System.IO.Error
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally, catchJust)
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

  (tr, tw, conn) <- atomically $ do
    tw <- newTChan
    tr <- newTChan
    tnn <- newTVar Nothing
    let c = Conn {
        cwrite = \m -> atomically $ writeTChan tw (Just m)
      , cread  = atomically $ readTChan tr
      , cclose = cclose' c tw tr
      , handle = h
      , hostName = hn
      , portNumber = pn
      , nickName = tnn }

    return (tr, tw, c)

  forkIO $ reader tr conn
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

reader :: (Msg b, Msg a) => TChan b -> Conn a b -> IO ()
reader tr conn = do
    catchJust (\e -> if isEOFError e then Just e else Nothing) (do
        msg <- hGetLine (handle conn)
        case fromStr msg of
            (Just msg') -> atomically $ writeTChan tr msg'
            Nothing     -> do
                             (cwrite conn) errMsg
        reader tr conn)
        eofHandler
  where
    eofHandler _ = do
      cclose conn


cclose' conn tw tr = do
    atomically $ do
        writeTChan tr eofMsg
        writeTChan tw Nothing
    -- TODO stop reader cleanly
    hClose (handle conn)
