module Cypher.JsonGen where

import Cypher.Types
import Control.Applicative
import Data.Aeson

-- TODO: Generate POST data for REST API endpoint
-- http://neo4j.com/docs/stable/rest-api-transactional.html

generate :: [Statement] -> String
generate statements = undefined
