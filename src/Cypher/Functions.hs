{-# LANGUAGE LambdaCase, OverloadedStrings, TypeOperators, RecordWildCards #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils
import Cypher.Urls
import Cypher.Actions
import Cypher.Request
import Debug.Trace

import Network.HTTP.Client
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Monad.Free
import Control.Monad.Reader
import Control.Applicative
import Data.Text.Encoding
import Data.HashMap.Lazy as L (empty)
import Data.Monoid((<>))
import qualified Data.HashMap.Strict as HM

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

deleteByUrl :: (Connection -> IO Request) -> Connection -> Neo4jAction r -> IO (Maybe r)
deleteByUrl url conn next = runRequest (json . delete) url conn >> interpret conn next

getProperty :: (Connection -> IO Request) -> Connection -> (Aeson.Value ~>  a) -> IO (Maybe a)
getProperty url conn next = do
    resp <- runRequest (json . get) url conn
    maybe (return Nothing) (interpret conn) (next <$> decodeBody resp)
        where wrap x = "{\"wrapped\":" <> x <> "}"
              unwrap :: Maybe (HM.HashMap T.Text Aeson.Value) -> Maybe Aeson.Value
              unwrap = (>>= HM.lookup "wrapped")
              decodeBody = unwrap . Aeson.decode . wrap . responseBody

setFromPayload load url conn next =
    runRequest (json . put . payload load) url conn >> interpret conn next

getRoot_ :: Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot_ = everythingOnAction interpret (json . get) baseUrl

listPropertyKeys_ :: Connection -> ([T.Text] ~> a) -> IO (Maybe a)
listPropertyKeys_ =
    everythingOnAction interpret (json . get) propertyKeysUrl

getNode_ :: Int -> Connection -> (NodeResponse ~> a) -> IO (Maybe a)
getNode_ nodeId =
 everythingOnAction interpret (json . get) (singleUrl nodeId)

createNode_ :: Maybe Props -> Connection -> (NodeResponse ~> a) -> IO (Maybe a)
createNode_ props =
    everythingOnAction interpret (json . post . maybeProps props) nodeUrl

deleteNode_ :: Int -> Connection -> Neo4jAction r -> IO (Maybe r)
deleteNode_ nodeId = deleteByUrl (singleUrl nodeId)

createRelationship_ :: Int -> Relationship -> Connection -> (RelationshipResponse ~> r) -> IO (Maybe r)
createRelationship_ nodeId rel =
    everythingOnAction interpret (json . post . payload rel) (relationshipUrl nodeId)

getRelationship_ :: Int -> Connection -> (RelationshipResponse ~> r) -> IO (Maybe r)
getRelationship_ relId =
    everythingOnAction interpret (json . get) (singleRelationshipUrl relId)

deleteRelationship_ :: Int -> Connection -> Neo4jAction r -> IO (Maybe r)
deleteRelationship_ relId = deleteByUrl (singleRelationshipUrl relId)

getRelationshipProperties_ :: Int -> Connection -> (Props ~> r) -> IO (Maybe r)
getRelationshipProperties_ relId =
    everythingOnAction interpret (json . get) (relationshipPropertiesUrl relId)

setRelationshipProperties_ :: Int -> Props -> Connection -> Neo4jAction r -> IO (Maybe r)
setRelationshipProperties_ relId props = setFromPayload props (relationshipPropertiesUrl relId)

getRelationshipProperty_ :: Int -> T.Text -> Connection -> (Props ~> a) -> IO (Maybe a)
getRelationshipProperty_ relId prop = getProperty (singleRelationshipPropertyUrl relId prop)

setRelationshipProperty_ :: Int -> T.Text -> T.Text -> Connection -> Neo4jAction r -> IO (Maybe r)
setRelationshipProperty_ relId key val =
    setFromPayload val (singleRelationshipPropertyUrl relId key)

getNodeRelationships_ :: Int -> RelType -> [T.Text] -> Connection -> ([RelationshipResponse] ~> r) -> IO (Maybe r)
getNodeRelationships_ nodeId relType types =
    everythingOnAction interpret (json . get) (nodeRelationshipsUrl nodeId relType types)

getRelationshipTypes_ :: Connection -> ([T.Text] ~> r) -> IO (Maybe r)
getRelationshipTypes_ =
    everythingOnAction interpret (json . get) relationshipTypesUrl

setNodeProperty_ :: Int -> Prop -> Props -> Connection -> Neo4jAction r -> IO (Maybe r)
setNodeProperty_ nodeId prop props =
    setFromPayload props (nodePropertyUrl nodeId prop)

setNodeProperties_ :: Int -> Props -> Connection -> (Props ~> r) -> IO (Maybe r)
setNodeProperties_ nodeId props =
    everythingOnAction interpret (json . put . payload props) (nodePropertiesUrl nodeId)

getNodeProperty_ :: Int -> Prop -> Connection -> (Props ~> r) -> IO (Maybe r)
getNodeProperty_ nodeId prop =
    getProperty (nodePropertyUrl nodeId prop)

deleteNodeProperties_ :: Int -> Connection -> Neo4jAction r -> IO (Maybe r)
deleteNodeProperties_ nodeId =
    deleteByUrl (nodePropertiesUrl nodeId)

deleteNodeProperty_ :: Int -> Prop -> Connection -> Neo4jAction r -> IO (Maybe r)
deleteNodeProperty_ nodeId prop =
    deleteByUrl (nodePropertyUrl nodeId prop)

interpret :: Connection -> Neo4jAction r -> IO (Maybe r)
interpret conn = \case
    Free action -> case action of
        ListPropertyKeys next -> listPropertyKeys_ conn next
        GetRoot next -> getRoot_ conn next
        GetNode nodeId next -> getNode_ nodeId conn next
        CreateNode props next -> createNode_ props conn next
        DeleteNode nodeId next -> deleteNode_ nodeId conn next
        GetRelationship relId next -> getRelationship_ relId conn next
        CreateRelationship nodeId rel next -> createRelationship_ nodeId rel conn next
        DeleteRelationship relId next -> deleteRelationship_ relId conn next
        GetRelationshipProperties relId next -> getRelationshipProperties_ relId conn next
        SetRelationshipProperties relId props next -> setRelationshipProperties_ relId props conn next
        GetRelationshipProperty relId prop next -> getRelationshipProperty_ relId prop conn next
        SetRelationshipProperty relId key val next -> setRelationshipProperty_ relId key val conn next
        GetNodeRelationships nodeId relType types next -> getNodeRelationships_ nodeId relType types conn next
        GetRelationshipTypes next -> getRelationshipTypes_ conn next
        SetNodeProperty nodeId prop props next -> setNodeProperty_ nodeId prop props conn next
        SetNodeProperties nodeId props next -> setNodeProperties_ nodeId props conn next
        GetNodeProperty nodeId prop next -> getNodeProperty_ nodeId prop conn next
        DeleteNodeProperties nodeId next -> deleteNodeProperties_ nodeId conn next
        DeleteNodeProperty nodeId prop next -> deleteNodeProperty_ nodeId prop conn next
        _ -> undefined
    Pure r -> return (Just r)

-- SOME TEST JUNK

testConnection :: IO Connection
testConnection = (<$> newManager defaultManagerSettings) (Connection "localhost" 7474)

test = do
    conn <- testConnection
    interpret conn (getNodeProperty 9 "foo")


