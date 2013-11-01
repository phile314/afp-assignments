{-# LANGUAGE ScopedTypeVariables #-}
module Afp.As4.ChClient
  ( runClient )
where

import System.IO
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Maybe
import Data.Map
import Afp.As4.ChConn
import System.IO.Error (isEOFError)

type ConnC = Conn C2SMsg String

-- | Run the client.
runClient :: (Maybe String) -> String -> Int -> IO ()
runClient nick hn pn = withSocketsDo $ do
  let pn' = fromIntegral pn
  h <- connectTo hn (PortNumber pn')

  n <- case nick of
            (Just n) -> return n
            Nothing -> do
                putStrLn "Please enter nick:"
                getLine

  putStrLn $ "Connecting with nick " ++ n ++ " ..."

  withConn (h, hn, pn') (start n) False


start :: String -> ConnC -> IO ()
start nick conn = do
    forkIO $ readLoop conn

    cwrite conn (S nick)

    writeLoop conn    


writeLoop conn = do
    str <- getLine
    case str of
        "exit" -> do
                    cwrite conn Exit
                    putStrLn "Exiting..."
                    cclose conn
        _      -> do
                    cwrite conn $ S str
                    writeLoop conn

readLoop conn =
    let f = do
            msg <- (cread conn)
            putStrLn msg
            readLoop conn
    in E.catchJust (\e -> if isEOFError e then Just e else Nothing) f (\e -> return ())

