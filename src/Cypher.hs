{-# LANGUAGE OverloadedStrings #-}

module Cypher where

import Data.Aeson
import Data.String
import Data.Monoid
import qualified Data.Text as T
import qualified Cypher.Types as C
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types.Header
import qualified Data.CaseInsensitive as CI

request :: C.Connection -> C.Neo4jRequest -> IO ()
request conn neo4jReq = do
    let url = "http://" <> (T.unpack (C.host conn)) <> ":" <> show (C.port conn) <> "/db/data/transaction/commit"
    req' <- parseUrl url
    let req = req' {
        method = "POST",
        requestHeaders = [(hContentType, "application/json")],
        requestBody = RequestBodyLBS (fromString $ show $ toJSON neo4jReq :: BL.ByteString)
    }
    return ()
