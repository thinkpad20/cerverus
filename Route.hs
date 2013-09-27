{-# LANGUAGE OverloadedStrings #-}
module Route where

import qualified Data.Map as M
import RFC2616
 -- hide RequestMethod so we can make one that derives Ord
import Network.HTTP.Base hiding (RequestMethod)
import qualified Data.ByteString as B

{- Mapping from cookies to usernames -}
type Connection = M.Map String String

{- Mapping from routes to handlers -}
type RouteMap = M.Map (RequestMethod,Uri) Handler

{- Later an environment will contain more -}
type Environment = Connection

{- A context encapsulates your routes as well, essentially 
   everything you need to run a web server
-}
type Context = (RouteMap, Environment)

data RequestMethod = HEAD
                   | GET
                   | POST
                   | PUT
                   | DELETE
                   | OPTIONS
                   | TRACE
                   | CONNECT
                   | Custom String
                   deriving (Eq, Ord, Show)

data Route = Route RequestMethod Uri Handler
data Request = Request RequestMethod Uri (Maybe Content)
type Content = (Headers, Data)
type Headers = M.Map B.ByteString B.ByteString
type Uri = B.ByteString
type Data = B.ByteString

type Handler = Request -> Environment -> Response

--newtype Response = Response (StatusCode, Content) deriving Show
type StatusCode = Int

toReqMethod :: B.ByteString -> RequestMethod
toReqMethod "GET" = GET
toReqMethod "POST" = POST
toReqMethod "PUT" = PUT
toReqMethod "DELETE" = DELETE
toReqMethod "OPTIONS" = OPTIONS
toReqMethod "TRACE" = TRACE
toReqMethod "HEAD" = HEAD
toReqMethod "CONNECT" = CONNECT
toReqMethod m = Custom m

respond :: StatusCode -> String -> Response
respond i s = Response $ (i, justData s)

emptyEnv :: Environment
emptyEnv = M.empty

justData :: Data -> Content
justData dta = ([], dta)

hello :: Handler
hello _ _ = respond 200 "Hello, world!"

routes :: [Route]
routes = [ Route GET "/hello" hello ] 

fromRoutes :: [Route] -> RouteMap
fromRoutes = foldr store M.empty where
  store :: Route -> RouteMap -> RouteMap
  store (Route method uri handler) mp = M.insert (method, uri) handler mp

getHandler :: Request -> RouteMap -> Maybe Handler
getHandler r@(Request m u c) = M.lookup (m,u)

handle :: Context -> Request -> Response
handle (routeMap, env) r = case getHandler r routeMap of
  Nothing -> respond 404 "Not found"
  Just h -> h r env

initContext = (fromRoutes routes, M.empty)

test1 = handle initContext $ Request GET "/hello" Nothing
test2 = handle initContext $ Request GET "/fgisdflk" Nothing