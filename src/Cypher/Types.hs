{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, DeriveFunctor #-}

module Cypher.Types
    ( Connection(..)
    , module Cypher.Types.Action
    , module Cypher.Types.Responses
    )
where

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Client

-- Re-exports
import Cypher.Types.Action
import Cypher.Types.Responses

data Connection = Connection {
    connHost :: T.Text,
    connPort :: Int,
    connManager :: Manager
}