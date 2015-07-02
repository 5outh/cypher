{-# LANGUAGE DeriveFunctor, TypeOperators #-}
module Cypher.Types.Action where

import Cypher.Types.Responses
import qualified Data.Text as T
import Control.Monad.Free

-- TODO: Finish building this out
-- | A Neo4j Action
data ActionF next =
      Authenticate T.Text T.Text (AuthResponse -> next)
    -- | GET /propertykeys
    | ListPropertyKeys ([T.Text] -> next)
    -- | GET /db/data/
    | GetRoot (RootResponse -> next)
    -- | GET /db/data/node/{Id}
    | GetNode Id (NodeResponse -> next)
    -- | POST /db/data/node
    | CreateNode (Maybe Props) (NodeResponse -> next)
    -- | DELETE /db/data/node/{Id}
    | DeleteNode Id next

    -- | PUT /db/data/node/{Id}/properties/{Prop}
    | SetNodeProperty Id Prop Props next
    -- | PUT /db/data/node/{Id}/properties
    | SetNodeProperties Id Props (Props -> next)
    -- | GET /db/data/node/{Id}/properties
    | GetNodeProperties Id (Props ~> next)
    -- | GET /db/data/node/{Id}/properties/{Prop}
    | GetNodeProperty Id Prop (Props -> next)
    -- | TODO: DELETE /db/data/node/{Id}/properties
    | DeleteNodeProperties Id next
    -- | TODO: DELETE /db/data/node/{Id}/properties/{Prop}
    | DeleteNodeProperty Id Prop next
    | AddNodeLabel Id Label
    | AddNodeLabels Id [Label]
    | ReplaceLabels Id [Label]
    | DeleteLabel Id Label
    | GetLabels Id [Label]
    | GetLabeledNodes Label
    --              Label-----v      v--Name v---Text Value
    | GetLabeledNodeWithProperty Label Prop Prop
    | GetAllLabels
    | GetNodeDegree Id RelType
    | GetNodeDegreeByType Id RelType [Typ]
    -- | /db/data/node/{Id}/relationships/{RelType}/{&List[Types]}
    | GetNodeRelationships Id RelType [T.Text] ([RelationshipResponse] -> next)

    -- | GET /db/data/relationship/{Id}
    | GetRelationship Id (RelationshipResponse -> next)
    -- | POST /db/data/node/{Id}/relationships
    | CreateRelationship Id Relationship (RelationshipResponse -> next)
    -- | DELETE /db/data/relationship/{Id}
    | DeleteRelationship Id next
    -- | GET /db/data/relationship/{Id}/properties
    | GetRelationshipProperties Id (Props -> next)
    -- | PUT /db/data/relationship/{Id}/properties
    | SetRelationshipProperties Id Props next
    | DeleteRelationshipProperties Id

    -- | GET /db/data/relationship/{Id}/properties/{Prop}
    | GetRelationshipProperty Id Prop (Props -> next)
    -- | POST /db/data/relationship/{Id}/properties/{Prop}
    | SetRelationshipProperty Id Prop Prop next
    | DeleteRelationshipProperty Id Prop

    -- | GET /db/data/relationship/types
    | GetRelationshipTypes ([T.Text] -> next)

    -- | Schema
    | CreateIndex T.Text [Prop] -- Index name, property_keys
    | DropIndex T.Text
    | CreateUniquenessConstraint Label [Prop] -- Label, Property Keys
    | GetUniquenessConstraint Label Prop
    | GetLabelConstraints Label
    | GetAllConstraints
    | DropConstraint Label Prop

    | RunShortestPath ShortestPathRequest
        deriving (Functor)

-- | A Neo4jAction is just a Free Monad
type Neo4jAction = Free ActionF

-- | Action from a to Neo4jAction, returning r
type a ~> r = a -> Neo4jAction r
