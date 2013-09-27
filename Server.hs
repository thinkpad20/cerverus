{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad                (forever)
import           Data.ByteString.Char8        hiding (putStrLn, putStr)
import qualified Data.ByteString.Char8        as BC
import           Network                      hiding (accept)
import           Network.Socket
import           Network.Socket.ByteString    (sendAll)
import           System.IO
import           System.IO.Streams            hiding (mapM_)
import           System.IO.Streams.Attoparsec (parseFromStream)
import           System.IO.Streams.Network    (socketToStreams)
import           System.Random
import           Data.Word
import           RFC2616                      (request)
import           RequestResponse              (requestMethod,
                                               requestVersion,
                                               requestUri)

data PortOption = RandomPort
                | DefaultPort
                | ThisPort PortNumber

defaultPort = PortNum 8080

getPortNum :: IO PortNumber
getPortNum = fmap PortNum $ getStdRandom (randomR (5000,6000))

start :: PortOption -> IO ()
-- generates a random port number
start RandomPort = getPortNum >>= \num -> startWithPort num
-- uses the default port number
start DefaultPort = startWithPort defaultPort
-- uses a specified port number
start (ThisPort num) = startWithPort num

startWithPort :: PortNumber -> IO ()
startWithPort num = withSocketsDo $ do
  sock <- listenOn $ PortNumber num
  putStrLn $ "Listening on port " ++ show num
  forever $ do
    (conn, addr) <- accept sock
    putStrLn $ "Connection received on " ++ show addr
    forkIO $ handle conn 

handle conn = do
  (is, os) <- socketToStreams conn
  (req, hdrs) <- parseFromStream request is
  putStr "Method: " >> print (requestMethod req)
  putStr "Version: " >> print (requestVersion req)
  putStr "URI: " >> print (requestUri req)
  putStr "Headers: " >> mapM_ print hdrs
  sendAll conn msg
  sClose conn
  where msg = "Boogey woogey!"

