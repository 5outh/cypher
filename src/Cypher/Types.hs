{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, DeriveFunctor #-}

module Cypher.Types where

import Data.Aeson
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Control.Monad.Free

data Statement = Statement {
    statement :: T.Text
} deriving (Show, Eq)

data Neo4jRequest = Neo4jRequest {
    statements :: [Statement]
} deriving (Show, Eq)

instance ToJSON Statement where
    toJSON (Statement stmt) = object ["statement" .= stmt]

instance FromJSON Statement where
    parseJSON (Object v) = Statement <$> v .: "statement"
    parseJSON _ = mzero

instance ToJSON Neo4jRequest where
    toJSON (Neo4jRequest stmts) = object ["statements" .= map toJSON stmts]

data Connection = Connection {
    host :: T.Text,
    port :: Int
} deriving (Show, Eq)

data Relationship = Relationship {
    to :: T.Text,
    typ :: T.Text,
    props :: Props
} deriving (Show, Eq)

instance ToJSON Relationship where
    toJSON (Relationship{..}) = object 
        [ "to" .= to
        , "type" .= typ
        , "data" .= props
        ]

data RelationshipDecl = RelationshipDecl {
    rdType :: T.Text,
    direction :: RelType
} deriving (Show, Eq)

instance ToJSON RelationshipDecl where
    toJSON (RelationshipDecl{..}) = object
        [ "type" .= rdType
        , "direction" .= show direction
        ]

data ShortestPathRequest = ShortestPathRequest {
    sprTo :: T.Text, -- URL
    maxDepth :: Int,
    relationships :: RelationshipDecl
} deriving (Show, Eq)

instance ToJSON ShortestPathRequest where
    toJSON (ShortestPathRequest{..}) = object
        [ "to" .= sprTo
        , "max_depth" .= maxDepth
        , "relationships" .= toJSON relationships
        , "algorithm" .= ("shortestPath" :: T.Text)
        ]

data DijkstraRequest = DijkstraRequest {
    drTo :: T.Text,
    costProperty :: T.Text,
    drRelationships :: RelationshipDecl
} deriving (Show, Eq)

instance ToJSON DijkstraRequest where 
    toJSON (DijkstraRequest{..}) = object 
        [ "to" .= drTo
        , "cost_property" .= costProperty
        , "relationships" .= toJSON drRelationships
        , "algorithm" .= ("dijkstra" :: T.Text)
        ]

-- | Avaliable Relationship Types
data RelType = All | In | Out deriving (Eq, Enum)

-- Used for JSON serialization
instance Show RelType where
    show All = "all"
    show In = "in"
    show Out = "out"

-- | A Neo4j Node Type
type Typ = T.Text

-- | A Neo4j Label
type Label = T.Text

-- | A Generic Neo4j Property
type Prop = T.Text

-- | Alias for a JSON object
type Props = Object

-- | Alias for Int
type Id = Int

data AuthResponse = AuthResponse {
    username :: T.Text,
    passwordChange :: T.Text,
    passwordChangeRequired :: T.Text
} deriving (Show, Eq)

instance FromJSON AuthResponse where
    parseJSON (Object v) = AuthResponse <$> v .: "username"
        <*> v .: "password_change"
        <*> v .: "password_change_required"
    parseJSON _ = mzero

data RootResponse = RootResponse {
    management :: T.Text,
    data' :: T.Text
} deriving (Show, Eq)

instance FromJSON RootResponse where
    parseJSON (Object v) = RootResponse <$> v .: "management"
        <*> v .: "data"
    parseJSON _ = mzero

-- | A Neo4j Action
data ActionF next = 
      Authenticate T.Text T.Text (AuthResponse -> next)
    | GetRoot (RootResponse -> next)
    -- | Node Actions
    | GetNode Id
    | CreateNode (Maybe Props)
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
    | GetRelationship Id
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

type Neo4jAction = Free ActionF