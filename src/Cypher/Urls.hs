{-# LANGUAGE OverloadedStrings #-}

module Cypher.Urls where

import Cypher.Types
import Cypher.Utils
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Client (parseUrl, Request(..))
import Control.Monad.Catch (MonadThrow(..))

parseEndpoint :: MonadThrow m => T.Text -> Connection -> m Request
parseEndpoint t = parseUrl . T.unpack . append t . base
    where base conn = "http://" <> connHost conn <> ":" <> (T.pack (show (connPort conn)))

baseUrl :: MonadThrow m => Connection -> m Request
baseUrl = parseEndpoint "/"

dataUrl :: MonadThrow m => Connection -> m Request
dataUrl = parseEndpoint "/db/data/"

nodeUrl :: MonadThrow m => Connection -> m Request
nodeUrl = parseEndpoint "/db/data/node/"

changePasswordUrl :: MonadThrow m => Connection -> m Request
changePasswordUrl = parseEndpoint "/user/neo4j/password/"

transactionUrl :: MonadThrow m => Connection -> m Request
transactionUrl = parseEndpoint "/db/data/transaction/"

commitUrl :: MonadThrow m => Connection -> m Request
commitUrl = parseEndpoint "/db/data/transaction/commit/"

singleUrl :: MonadThrow m => Int -> Connection -> m Request
singleUrl x =  parseEndpoint ("/db/data/node/" <> ident x)

singleRelationshipUrl :: MonadThrow m => Int -> Connection -> m Request
singleRelationshipUrl x = parseEndpoint ("/db/data/relationship/" <> ident x)

relationshipPropertiesUrl :: MonadThrow m => Int -> Connection -> m Request
relationshipPropertiesUrl x = parseEndpoint ("/db/data/relationship/" <> ident x <> "/properties")

singleRelationshipPropertyUrl :: MonadThrow m => Int -> T.Text -> Connection -> m Request
singleRelationshipPropertyUrl relId prop =
    parseEndpoint ("/db/data/relationship/" <> ident relId <> "/properties/" <> prop)

relationshipUrl :: MonadThrow m => Int -> Connection -> m Request
relationshipUrl startNodeId = parseEndpoint ("/db/data/node/" <> ident startNodeId <> "/relationships")

nodeRelationshipsUrl :: MonadThrow m => Int -> RelType -> [T.Text] -> Connection -> m Request
nodeRelationshipsUrl nodeId relType types =
    parseEndpoint $ mconcat [ "/db/data/node/"
                            , ident nodeId
                            , "/relationships/"
                            , T.pack (show relType)
                            , "/"
                            , T.intercalate "&" types
                            ]

propertyKeysUrl :: MonadThrow m => Connection -> m Request
propertyKeysUrl = parseEndpoint "/db/data/propertykeys"