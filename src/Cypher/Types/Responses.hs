{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, DeriveFunctor #-}
module Cypher.Types.Responses where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Free

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
type Props = Value

-- | Alias for Int
type Id = Int

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

data NodeMetadata = NodeMetadata {
    nodeId :: Int,
    nodeLabels :: [T.Text]
} deriving (Show, Eq)

instance FromJSON NodeMetadata where
    parseJSON (Object v) = NodeMetadata <$> v .: "id" <*> v .: "labels"
    parseJSON _ = mzero

-- NOTE: Links apart from `self` are not included; we can reconstruct them from the id.
data NodeResponse = NodeResponse {
    nodeExtensions :: Object,
    nodeSelf :: T.Text,
    nodeMetadata :: NodeMetadata,
    nodeData :: Object -- NOTE: Given a way to parse THIS into a Haskell object, we can update etc.
} deriving (Show, Eq)

instance FromJSON NodeResponse where
    parseJSON (Object v) = NodeResponse <$> v .: "extensions"
        <*> v .: "self"
        <*> v .: "metadata"
        <*> v .: "data"
    parseJSON _ = mzero

data RelationshipMetadata = RelationshipMetadata {
    relId :: Int,
    relType :: T.Text
} deriving (Show, Eq)

instance FromJSON RelationshipMetadata where
    parseJSON (Object v) = RelationshipMetadata <$> v .: "id" <*> v .: "type"
    parseJSON _ = mzero

data RelationshipResponse = RelationshipResponse {
    relExtensions :: Object,
    relSelf :: T.Text,
    relStart :: T.Text, -- Start Node URL
    relEnd :: T.Text, -- End Node URL
    relMetadata :: RelationshipMetadata,
    relData :: Object
} deriving (Show, Eq)

instance FromJSON RelationshipResponse where
    parseJSON (Object v) = RelationshipResponse <$> v .: "extensions"
        <*> v .: "self"
        <*> v .: "start"
        <*> v .: "end"
        <*> v .: "metadata"
        <*> v .: "data"
    parseJSON _ = mzero