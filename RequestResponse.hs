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

type RequestMethod = ByteString
get, post, put, delete :: RequestMethod
get = "GET"
post = "POST"
put = "PUT"
delete = "DELETE"

type HttpVersion = ByteString
http09, http10, http11 :: HttpVersion
http09 = "0.9"
http10 = "1.0"
http11 = "1.1"

type QueryParam = ByteString
type QueryArg = ByteString
type Queries = M.Map QueryParam (Maybe QueryArg)
data Uri = Uri {
      getPath :: ByteString
    , getQueries :: Queries
    } deriving (Eq, Ord, Show)

bstrToUri :: ByteString -> Uri
bstrToUri uriStr = case B8.split '?' fullUri of
  [] -> Uri "" M.empty
  [path] -> Uri path M.empty
  (path:kvs:_) -> Uri path (process kvs)
  where getPair (a:b:_) = (a, Just b)
        getPair [a] = (a, Nothing)
        getPair [] = ("", Nothing)
        process = M.fromList . map getPair . map (B8.split '=') . B8.split '&'

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