{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Cypher.Actions where

import           Cypher.Types
import           Cypher.Utils
import           Data.Monoid

import           Control.Monad.Free
import           Data.Text          as T

commitTransaction :: Neo4jRequest ~> TransactionResponse
commitTransaction req = liftFn (CommitTransaction req)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id

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

getRelationshipProperty :: Int -> Prop ~> Props
getRelationshipProperty relId prop = liftFn (GetRelationshipProperty relId prop)

setRelationshipProperties :: Int -> Props ~> ()
setRelationshipProperties relId props = liftF (SetRelationshipProperties relId props ())

setRelationshipProperty :: Int -> Prop -> Prop ~> ()
setRelationshipProperty relId propKey propVal = liftF (SetRelationshipProperty relId propKey propVal ())

getNodeRelationships :: Int -> RelType -> [T.Text] ~> [RelationshipResponse]
getNodeRelationships nodeId relType types = liftFn (GetNodeRelationships nodeId relType types)

getRelationshipTypes :: Neo4jAction [T.Text]
getRelationshipTypes = liftFn GetRelationshipTypes

setNodeProperty :: Id -> Prop -> Props ~> ()
setNodeProperty nodeId prop props = liftF (SetNodeProperty nodeId prop props ())

setNodeProperties :: Id -> Props ~> Props
setNodeProperties nodeId prop = liftFn (SetNodeProperties nodeId prop)

getNodeProperty :: Id -> Prop ~> Props
getNodeProperty nodeId prop = liftFn (GetNodeProperty nodeId prop)

deleteNodeProperty :: Id -> Prop ~> ()
deleteNodeProperty nodeId prop = liftF (DeleteNodeProperty nodeId prop ())

deleteNodeProperties :: Id ~> ()
deleteNodeProperties nodeId = liftF (DeleteNodeProperties nodeId ())

findNodesByProp :: T.Text -> T.Text -> Neo4jAction TransactionResponse
findNodesByProp key val = do
    let stmt = "MATCH (n{ " <> key <> ":\"" <> val <> "\"}) RETURN n"
    commitTransaction (Neo4jRequest [Statement stmt] Nothing)
