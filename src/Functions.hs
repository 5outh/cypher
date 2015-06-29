{-# LANGUAGE LambdaCase, OverloadedStrings, TypeOperators #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils

import qualified Data.Aeson as Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Monad.Free
import Network.HTTP.Types.Header
import Control.Applicative
import Data.Text.Encoding

-- TODO: Refactor into Reader w/ Conn/Manager
-- TODO: Move things around

type Endo a = a -> a

liftFn :: (MonadFree f m, Functor f) => ((a -> a) -> f b) -> m b
liftFn f = liftF (f id)

authenticate :: T.Text -> T.Text -> Neo4jAction AuthResponse
authenticate user pass = liftFn (Authenticate user pass)

getNode :: Int -> Neo4jAction NodeResponse
getNode nodeId = liftFn (GetNode nodeId)

createNode :: Maybe Props -> Neo4jAction NodeResponse
createNode props = liftFn (CreateNode props)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id

addHeader :: Header -> Endo Request
addHeader header req = req { requestHeaders = header: requestHeaders req }

json :: Endo Request
json = addHeader (hContentType, "application/json")

get :: Endo Request
get req = req { method = "GET" }

post :: Endo Request
post req = req { method = "POST" }

maybeProps :: Maybe Props -> Endo Request
maybeProps props req = case props of
    Nothing -> req
    Just props' -> req { requestBody = RequestBodyLBS (Aeson.encode props') }

body :: Aeson.FromJSON a => Response LB.ByteString -> Maybe a
body = Aeson.decode . responseBody

runRequest :: (a -> Request) -> (b -> IO a) -> Manager -> b -> IO (Response LB.ByteString)
runRequest endo url manager conn = (`httpLbs` manager) =<< endo <$> url conn

everythingOnAction
  :: Aeson.FromJSON c =>
     (Manager -> t -> b -> IO (Maybe d))
     -> (a -> Request)
     -> (t -> IO a)
     -> Manager
     -> t
     -> (c -> b)
     -> IO (Maybe d)
everythingOnAction doNext endo url manager conn next = do
    resp <- runRequest endo url manager conn
    maybe (return Nothing) (doNext manager conn) (next <$> body resp)

getRoot_ :: Manager -> Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot_ manager conn next =
    everythingOnAction interpret (json . get) baseUrl manager conn next

getNode_ :: Manager -> Connection -> Int -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
getNode_ manager conn nodeId next =
 everythingOnAction interpret (json . get) (singleUrl nodeId) manager conn next

createNode_ :: Manager -> Connection -> Maybe Props -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
createNode_ manager conn props next =
    everythingOnAction interpret (json . post . maybeProps props) nodeUrl manager conn next

interpret :: Manager -> Connection -> Neo4jAction r -> IO (Maybe r)
interpret manager conn = \case
    Free action -> case action of
        GetRoot next -> getRoot_ manager conn next
        GetNode nodeId next -> getNode_ manager conn nodeId next
        CreateNode props next -> createNode_ manager conn props next
        _ -> undefined
    Pure r -> return (Just r)
