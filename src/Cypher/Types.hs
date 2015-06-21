{-# LANGUAGE OverloadedStrings #-}

module Cypher.Types (Statement(..)) where

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
