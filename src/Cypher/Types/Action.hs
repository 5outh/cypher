{-# LANGUAGE DeriveFunctor, TypeOperators #-}
module Cypher.Types.Action where

import Cypher.Types.Responses
import qualified Data.Text as T
import Control.Monad.Free

-- TODO: Finish building this out
-- | A Neo4j Action
data ActionF next =
      Authenticate T.Text T.Text (AuthResponse -> next)
    | ListPropertyKeys ([T.Text] -> next)
    | GetRoot (RootResponse -> next)
    -- | Node Actions
    | GetNode Id (NodeResponse -> next)
    | CreateNode (Maybe Props) (NodeResponse -> next)
    | DeleteNode Id next

    | SetNodeProperty Id Prop Props
    | SetNodeProperies Id Props
    | GetNodeProperties Id
    | GetNodeProperty Id Prop
    | DeleteNodeProperties Id
    | DeleteNodeProperty Id Prop
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
    | GetNodeRelationships Id RelType [T.Text] ([RelationshipResponse] -> next)

    -- | Relationship Actions
    | GetRelationship Id (RelationshipResponse -> next)
    -- NB. Id is starting node's ID
    | CreateRelationship Id Relationship (RelationshipResponse -> next)
    | DeleteRelationship Id next
    | GetRelationshipProperties Id (Props -> next)
    | SetRelationshipProperties Id Props next
    | DeleteRelationshipProperties Id

    | GetRelationshipProperty Id Prop (T.Text -> next)
    | SetRelationshipProperty Id Prop Prop next
    | DeleteRelationshipProperty Id Prop

    | GetRelationships RelType [Typ]
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
