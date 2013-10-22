module Afp.As4.ChServer
  ( run )
where

import Afp.As4.Messages
import System.IO
import Network
import Control.Concurrent

data ChatState = ChatState ()

run :: IO ()
run = runServer 9595

runServer :: Int -> IO ()
runServer p = withSocketsDo $ do
  s <- listenOn (PortNumber (fromIntegral p))
  mainLoop undefined s
  -- TODO error handling

mainLoop :: ChatState -> Socket -> IO ()
mainLoop cs s = do
  conn <- accept s
  forkIO $ handleClient cs conn
  mainLoop cs s

handleClient :: ChatState -> (Handle, HostName, PortNumber) -> IO ()
handleClient cs (h, _, _) = do
  hSetBuffering h NoBuffering
--  handleMsgs h

{-handleMsgs :: Handle -> Handler -> IO ()
handleMsgs h hr = do
  ln <- hGetLine h
  let msg = (read ln) :: MsgC2S
  hr msg-}
