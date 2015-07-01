{-# LANGUAGE TypeOperators #-}
module Cypher.Actions where

import Cypher.Types
import Cypher.Utils

import Control.Monad.Free
import Data.Text as T

authenticate :: T.Text -> T.Text ~> AuthResponse
authenticate user pass = liftFn (Authenticate user pass)

getNode :: Int ~> NodeResponse
getNode nodeId = liftFn (GetNode nodeId)

createNode :: Maybe Props ~> NodeResponse
createNode props = liftFn (CreateNode props)

-- NOTE: Nodes with relationships cannot be deleted.
deleteNode :: Int ~> ()
deleteNode nodeId = liftF (DeleteNode nodeId ())

listPropertyKeys :: Neo4jAction [T.Text]
listPropertyKeys = liftFn ListPropertyKeys

getRelationship :: Int ~> RelationshipResponse
getRelationship relId = liftFn (GetRelationship relId)

createRelationship :: Int -> Relationship ~> RelationshipResponse
createRelationship nodeId rel = liftFn (CreateRelationship nodeId rel)

deleteRelationship :: Int ~> ()
deleteRelationship relId = liftF (DeleteRelationship relId ())

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id