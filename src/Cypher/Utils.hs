{-# LANGUAGE OverloadedStrings #-}

module Cypher.Utils where

import Cypher.Types
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Base64 as Base64
import Network.HTTP.Client (parseUrl, Request(..))
import Control.Monad.Catch (MonadThrow(..))

append :: Monoid a => a -> a -> a
append = flip (<>)

ident :: Int -> T.Text
ident x = T.pack $ show x

parseEndpoint t = parseUrl . T.unpack . append t . base

base :: Connection -> T.Text
base conn = "http://" <> connHost conn <> ":" <> (T.pack (show (connPort conn)))

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
