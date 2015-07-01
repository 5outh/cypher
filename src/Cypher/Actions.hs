module Cypher.Actions where

import Cypher.Types
import Cypher.Utils

import Control.Monad.Free
import Data.Text as T

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

createRelationship :: Int -> Relationship -> Neo4jAction RelationshipResponse
createRelationship nodeId rel = liftFn (CreateRelationship nodeId rel)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id