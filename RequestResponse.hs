{-# LANGUAGE OverloadedStrings #-}
module RequestResponse where

import Data.ByteString hiding (map)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M

 -- hide RequestMethod so we can make one that derives Ord
import Network.HTTP.Base hiding (RequestMethod(..),
                                 Request)

f .> g = g . f
a ==> f = f a

data RequestMethod = HEAD
                   | GET
                   | POST
                   | PUT
                   | DELETE
                   | OPTIONS
                   | TRACE
                   | CONNECT
                   | Custom ByteString
                   deriving (Eq, Ord, Show)

toReqMethod :: ByteString -> RequestMethod
toReqMethod m = case m of
  "GET"     -> GET
  "POST"    -> POST
  "PUT"     -> PUT
  "DELETE"  -> DELETE
  "OPTIONS" -> OPTIONS
  "TRACE"   -> TRACE
  "HEAD"    -> HEAD
  "CONNECT" -> CONNECT
  otherwise -> Custom m

http09, http10, http11 :: ByteString
http09 = "0.9"
http10 = "1.0"
http11 = "1.1"

type Params = M.Map ByteString ByteString
newtype Uri = Uri (ByteString, Params) deriving (Eq, Ord, Show)

toUri :: ByteString -> Uri
toUri fullUri = case B8.split '?' fullUri of
  [] -> Uri ("", M.empty)
  [uri] -> Uri (uri, M.empty)
  (uri:kvs:_) -> Uri (uri, kvs ==> (B8.split '&' .> 
                                     map (B8.split '=') .>
                                     map getPair .> 
                                     M.fromList))
  where getPair (a:b:_) = (a,b)
        getPair [a] = (a,"")
        getPair [] = ("","")

mkRequest :: ByteString -> ByteString -> ByteString -> Request
mkRequest method uri version = Request m u v where
  m = toReqMethod method
  u = toUri uri
  v = version


data Header = Header {
      headerName  :: !ByteString
    , headerValue :: [ByteString]
    } deriving (Eq, Ord, Show)

data Request = Request {
      requestMethod  :: !RequestMethod
    , requestUri     :: !Uri
    , requestVersion :: !ByteString
    } deriving (Eq, Ord, Show)

data Response = Response {
      responseVersion :: !ByteString
    , responseCode    :: !ByteString
    , responseMsg     :: !ByteString
    } deriving (Eq, Ord, Show)