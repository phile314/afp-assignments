module Afp.As4.ChClient
  ( runClient, runClient' )
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
import Afp.As4.ChConn


type ConnC = Conn MsgC2S MsgS2C


runClient' :: String -> IO ()
runClient' nick = runClient nick "localhost" 9595

runClient :: String -> String -> Int -> IO ()
runClient nick hn pn = withSocketsDo $ do
  let pn' = fromIntegral pn
  h <- connectTo hn (PortNumber pn')

  withConn (h, hn, pn') (start nick)

start :: String -> ConnC -> IO ()
start nick conn = do
    forkIO $ readLoop conn

    cwrite conn (CJoin nick)

    writeLoop conn    

writeLoop conn = do
    str <- getLine
    case str of
        "exit" -> do
                    cclose conn
        _      -> do
                    cwrite conn (CSpeak str)
                    writeLoop conn

readLoop conn = do
    msg <- (cread conn)
    putStrLn (toStr msg)
    readLoop conn


{-cread' :: ChatState' -> ConnS -> (MsgC2S -> IO ()) -> IO ()
cread' cs conn cont = do
    msg <- cread conn
    case msg of
        SEOF -> do
                    putStrLn "Lost server connection, terminating..."
        a    -> cont a
    

-}


