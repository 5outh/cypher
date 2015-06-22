{-# LANGUAGE OverloadedStrings #-}

module Cypher.Types where

import Data.Aeson
import qualified Data.Text as T
import Control.Applicative
import Control.Monad

data Statement = Statement {
    statement :: T.Text
}

instance ToJSON Statement where
    toJSON (Statement stmt) = object ["statement" .= stmt]

instance FromJSON Statement where
    parseJSON (Object v) = Statement <$> v .: "statement"
    parseJSON _ = mzero

data Neo4jRequest = Neo4jRequest {
    statements :: [Statement]
}

instance ToJSON Neo4jRequest where
    toJSON (Neo4jRequest stmts) = object ["statements" .= map toJSON stmts]

data Connection = Connection {
    host :: T.Text,
    port :: Int
}
