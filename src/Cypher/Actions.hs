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

-- NOTE: This is a little weird, since the start node is encoded in the relationship itself.
createRelationship :: Int -> Relationship ~> RelationshipResponse
createRelationship nodeId rel = liftFn (CreateRelationship nodeId rel)

deleteRelationship :: Int ~> ()
deleteRelationship relId = liftF (DeleteRelationship relId ())

getRelationshipProperties :: Int ~> Props
getRelationshipProperties relId = liftFn (GetRelationshipProperties relId)

getRelationshipProperty :: Int -> Prop ~> Prop
getRelationshipProperty relId prop = liftFn (GetRelationshipProperty relId prop)

setRelationshipProperties :: Int -> Props ~> ()
setRelationshipProperties relId props = liftF (SetRelationshipProperties relId props ())

setRelationshipProperty :: Int -> Prop -> Prop ~> ()
setRelationshipProperty relId propKey propVal = liftF (SetRelationshipProperty relId propKey propVal ())

getNodeRelationships :: Int -> RelType -> [T.Text] ~> [RelationshipResponse]
getNodeRelationships nodeId relType types = liftFn (GetNodeRelationships nodeId relType types)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id