module Main where

import System.Environment
import Afp.As4.ChServer
import Afp.As4.ChClient

main :: IO ()
main = do
    args <- getArgs
    case args of
        "client":host:port:rem -> runClient (if null rem then Nothing else Just $ head rem) host (read port)
        "server":port:_        -> runServer (read port)
