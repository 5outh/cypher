{-# LANGUAGE LambdaCase, OverloadedStrings, TypeOperators, RecordWildCards #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils
import Cypher.Urls

import qualified Data.Aeson as Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.Free
import Network.HTTP.Types.Header
import Control.Applicative
import Data.Text.Encoding

-- TODO: Refactor into Reader w/ Conn/Manager
-- TODO: Move things around
-- TODO: Refactor from Maybe into ErrorResponse | Response

authenticate :: T.Text -> T.Text -> Neo4jAction AuthResponse
authenticate user pass = liftFn (Authenticate user pass)

getNode :: Int -> Neo4jAction NodeResponse
getNode nodeId = liftFn (GetNode nodeId)

createNode :: Maybe Props -> Neo4jAction NodeResponse
createNode props = liftFn (CreateNode props)

-- NOTE: Nodes with relationships cannot be deleted.
deleteNode :: Int -> Neo4jAction ()
deleteNode nodeId = liftF (DeleteNode nodeId ())

listPropertyKeys :: Neo4jAction [T.Text]
listPropertyKeys = liftFn ListPropertyKeys

getRelationship :: Int -> Neo4jAction RelationshipResponse
getRelationship relId = liftFn (GetRelationship relId)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id

addHeader :: Header -> Endo Request
addHeader header req = req { requestHeaders = header: requestHeaders req }

json :: Endo Request
json = addHeader (hContentType, "application/json")

setMethod :: B.ByteString -> Endo Request
setMethod typ req = req{ method = typ}

get :: Endo Request
get = setMethod "GET"

post :: Endo Request
post = setMethod "POST"

delete :: Endo Request
delete = setMethod "DELETE"

maybeProps :: Maybe Props -> Endo Request
maybeProps props req = case props of
    Nothing -> req
    Just props' -> req { requestBody = RequestBodyLBS (Aeson.encode props') }

body :: Aeson.FromJSON a => Response LB.ByteString -> Maybe a
body = Aeson.decode . responseBody

runRequest :: (a -> Request) -> (Connection -> IO a) -> Connection -> IO (Response LB.ByteString)
runRequest endo url conn@Connection{..} = (`httpLbs` connManager) =<< endo <$> url conn

everythingOnAction
  :: Aeson.FromJSON a3 =>
     (Connection -> a1 -> IO (Maybe a2))
     -> (a -> Request)
     -> (Connection -> IO a)
     -> Connection
     -> (a3 -> a1)
     -> IO (Maybe a2)
everythingOnAction doNext endo url conn@Connection{..} next = do
    resp <- runRequest endo url conn
    maybe (return Nothing) (doNext conn) (next <$> body resp)

getRoot_ :: Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot_ conn next =
    everythingOnAction interpret (json . get) baseUrl conn next

listPropertyKeys_ :: Connection -> ([T.Text] -> Neo4jAction a) -> IO (Maybe a)
listPropertyKeys_ conn next =
    everythingOnAction interpret (json . get) propertyKeysUrl conn next

getNode_ :: Connection -> Int -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
getNode_ conn nodeId next =
 everythingOnAction interpret (json . get) (singleUrl nodeId) conn next

createNode_ :: Connection -> Maybe Props -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
createNode_ conn props next =
    everythingOnAction interpret (json . post . maybeProps props) nodeUrl conn next

deleteNode_ :: Connection -> Int -> Neo4jAction r -> IO (Maybe r)
deleteNode_ conn nodeId next = do
    runRequest (json . delete) (singleUrl nodeId) conn
    interpret conn next

getRel_ conn relId next =
    everythingOnAction interpret (json . get) (singleRelationshipUrl relId) conn next

interpret :: Connection -> Neo4jAction r -> IO (Maybe r)
interpret conn = \case
    Free action -> case action of
        ListPropertyKeys next -> listPropertyKeys_ conn next
        GetRoot next -> getRoot_ conn next
        GetNode nodeId next -> getNode_ conn nodeId next
        CreateNode props next -> createNode_ conn props next
        DeleteNode nodeId next -> deleteNode_ conn nodeId next
        GetRelationship relId next -> getRel_ conn relId next
        _ -> undefined
    Pure r -> return (Just r)

newtype Neo4j a = Neo4j{ runNeo4j :: IO (Maybe a) }

testConnection :: IO Connection
testConnection = (<$> newManager defaultManagerSettings) (Connection "localhost" 7474)
