{-# LANGUAGE LambdaCase, OverloadedStrings, TypeOperators, RecordWildCards #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils
import Cypher.Urls
import Cypher.Actions
import Cypher.Request
import Debug.Trace

import Network.HTTP.Client
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Monad.Free
import Control.Monad.Reader
import Control.Applicative
import Data.Text.Encoding
import Data.HashMap.Lazy as L (empty)

-- TODO: Refactor into Reader w/ Conn/Manager
-- TODO: Move things around
-- TODO: Refactor from Maybe into ErrorResponse | Response

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

deleteByUrl conn url next = runRequest (json . delete) url conn >> interpret conn next

getRoot_ :: Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot_ conn next =
    everythingOnAction interpret (json . get) baseUrl conn next

listPropertyKeys_ :: Connection -> ([T.Text] ~> a) -> IO (Maybe a)
listPropertyKeys_ conn next =
    everythingOnAction interpret (json . get) propertyKeysUrl conn next

getNode_ :: Connection -> Int -> (NodeResponse ~> a) -> IO (Maybe a)
getNode_ conn nodeId next =
 everythingOnAction interpret (json . get) (singleUrl nodeId) conn next

createNode_ :: Connection -> Maybe Props -> (NodeResponse ~> a) -> IO (Maybe a)
createNode_ conn props next =
    everythingOnAction interpret (json . post . maybeProps props) nodeUrl conn next

deleteNode_ :: Connection -> Int -> Neo4jAction r -> IO (Maybe r)
deleteNode_ conn nodeId next = do
    runRequest (json . delete) (singleUrl nodeId) conn
    interpret conn next

createRelationship_ :: Connection -> Int -> Relationship -> (RelationshipResponse ~> r) -> IO (Maybe r)
createRelationship_ conn nodeId rel =
    everythingOnAction interpret (json . post . payload rel) (relationshipUrl nodeId) conn

getRelationship_ :: Connection -> Int -> (RelationshipResponse ~> r) -> IO (Maybe r)
getRelationship_ conn relId =
    everythingOnAction interpret (json . get) (singleRelationshipUrl relId) conn

deleteRelationship_ :: Connection -> Int -> Neo4jAction r -> IO (Maybe r)
deleteRelationship_ conn relId = deleteByUrl conn (singleRelationshipUrl relId)

getRelationshipProperties_ :: Connection -> Int -> (Props ~> r) -> IO (Maybe r)
getRelationshipProperties_ conn relId =
    everythingOnAction interpret (json . get) (relationshipPropertiesUrl relId) conn

setRelationshipProperties_ :: Connection -> Int -> Props -> Neo4jAction r -> IO (Maybe r)
setRelationshipProperties_ conn relId props next = do
    runRequest (json . put . payload props) (relationshipPropertiesUrl relId) conn
    interpret conn next

-- | TODO: Needs FromJSON
getRelationshipProperty_ :: Connection -> Int -> T.Text -> (T.Text ~> a) -> IO (Maybe a)
getRelationshipProperty_ conn relId prop next = do
    resp <- runRequest (json . get) (singleRelationshipPropertyUrl relId prop) conn
    maybe (return Nothing) (interpret conn) (Just . next $ decodeBody resp)
        where decodeBody = decodeUtf8 . LB.toStrict . responseBody

setRelationshipProperty_ :: Connection -> Int -> T.Text -> T.Text -> Neo4jAction r -> IO (Maybe r)
setRelationshipProperty_ conn relId key val next = do
    runRequest (json . put . payload val) (singleRelationshipPropertyUrl relId key) conn
    interpret conn next

getNodeRelationships_ :: Connection -> Int -> RelType -> [T.Text] -> ([RelationshipResponse] ~> r) -> IO (Maybe r)
getNodeRelationships_ conn nodeId relType types =
    everythingOnAction interpret (json . get) (nodeRelationshipsUrl nodeId relType types) conn

interpret :: Connection -> Neo4jAction r -> IO (Maybe r)
interpret conn = \case
    Free action -> case action of
        ListPropertyKeys next -> listPropertyKeys_ conn next
        GetRoot next -> getRoot_ conn next
        GetNode nodeId next -> getNode_ conn nodeId next
        CreateNode props next -> createNode_ conn props next
        DeleteNode nodeId next -> deleteNode_ conn nodeId next
        GetRelationship relId next -> getRelationship_ conn relId next
        CreateRelationship nodeId rel next -> createRelationship_ conn nodeId rel next
        DeleteRelationship relId next -> deleteRelationship_ conn relId next
        GetRelationshipProperties relId next -> getRelationshipProperties_ conn relId next
        SetRelationshipProperties relId props next -> setRelationshipProperties_ conn relId props next
        GetRelationshipProperty relId prop next -> getRelationshipProperty_ conn relId prop next
        SetRelationshipProperty relId key val next -> setRelationshipProperty_ conn relId key val next
        GetNodeRelationships nodeId relType types next -> getNodeRelationships_ conn nodeId relType types next
        _ -> undefined
    Pure r -> return (Just r)

-- SOME TEST JUNK

testConnection :: IO Connection
testConnection = (<$> newManager defaultManagerSettings) (Connection "localhost" 7474)

testRel = Relationship "http://localhost:7474/db/data/node/5" "TEST" (Aeson.object [])
