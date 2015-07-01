{-# LANGUAGE OverloadedStrings #-}
module Cypher.Request where

import Cypher.Utils
import Cypher.Types.Responses

import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Monoid ((<>))

addHeader :: Header -> Endo Request
addHeader header req = req { requestHeaders = header: requestHeaders req }

json :: Endo Request
json = addHeader (hContentType, "application/json")

setMethod :: B.ByteString -> Endo Request
setMethod typ req = req{ method = typ}

get :: Endo Request
get = setMethod "GET"

post :: Endo Request
post = setMethod "POST"

put :: Endo Request
put = setMethod "PUT"

delete :: Endo Request
delete = setMethod "DELETE"

maybeProps :: Maybe Props -> Endo Request
maybeProps props req = case props of
    Nothing -> req
    Just props' -> req { requestBody = RequestBodyLBS (Aeson.encode props') }

payload :: Aeson.ToJSON a => a -> Endo Request
payload ps req = req { requestBody = RequestBodyLBS (Aeson.encode ps) }

-- | NOTE: `str` needs to be encoded as a JSON value
payloadRaw :: T.Text -> Endo Request
payloadRaw str req = req { requestBody = RequestBodyLBS (LB.fromStrict $ encodeUtf8 str) }
