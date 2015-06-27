{-# LANGUAGE OverloadedStrings #-}

module Cypher.Utils where

import Cypher.Types
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Base64 as Base64

append :: Monoid a => a -> a -> a
append = flip (<>)

ident :: Int -> T.Text
ident x = T.pack $ show x

baseUrl :: Connection -> T.Text
baseUrl conn = "http://" <> host conn <> ":" <> (T.pack (show (port conn)))

endpoint :: T.Text -> Connection -> T.Text
endpoint x = append x . baseUrl

dataUrl :: Connection -> T.Text
dataUrl = endpoint "/db/data"

changePasswordUrl :: Connection -> T.Text
changePasswordUrl = endpoint "/user/neo4j/password"

transaction :: Connection -> T.Text
transaction = append "/transaction" . dataUrl

commitUrl :: Connection -> T.Text
commitUrl = append "/commit" . transaction

transactionUrl :: Connection -> T.Text
transactionUrl = transaction

singleUrl :: Int -> Connection -> T.Text
singleUrl x = endpoint ("/" <> ident x) 

singleCommitUrl :: Int -> Connection -> T.Text
singleCommitUrl x = append "/commit" . singleUrl x

-- | Generate authentication key for User/Password (for Authorization Header)
authKey :: B.ByteString -> B.ByteString -> T.Text
authKey user pass = "Basic " <> T.pack token
    where token = C8.unpack (Base64.encode $ user <> ":" <> pass)
