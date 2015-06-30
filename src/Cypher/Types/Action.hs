{-# LANGUAGE DeriveFunctor #-}
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


    -- | Relationship Actions
    | GetRelationship Id (RelationshipResponse -> next)
    | CreateRelationship Id [RelType] (Maybe Props)
    | DeleteRelationship Id
    | GetRelationshipProperties Id
    | SetRelationshipProperties Id Props
    | DeleteRelationshipProperties Id

    | GetRelationshipProperty Id Prop
    | DeleteRelationshipProperty Id Prop

    | GetRelationships RelType [Typ]
    | GetRelationshipTypes

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
