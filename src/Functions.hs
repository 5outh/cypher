{-# LANGUAGE LambdaCase, OverloadedStrings, TypeOperators #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils

import qualified Data.Aeson as Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Monad.Free
import Network.HTTP.Types.Header
import Control.Applicative
import Data.Text.Encoding

type Endo a = a -> a

liftFn :: (MonadFree f m, Functor f) => ((a -> a) -> f b) -> m b
liftFn f = liftF (f id)

authenticate :: T.Text -> T.Text -> Neo4jAction AuthResponse
authenticate user pass = liftFn (Authenticate user pass)

getNode :: Int -> Neo4jAction NodeResponse
getNode nodeId = liftFn (GetNode nodeId)

createNode :: Maybe Props -> Neo4jAction NodeResponse
createNode props = liftFn (CreateNode props)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id

addHeader :: Header -> Endo Request
addHeader header req = req { requestHeaders = header: requestHeaders req }

json :: Endo Request
json = addHeader (hContentType, "application/json")

get :: Endo Request
get req = req { method = "GET" }

post :: Endo Request
post req = req { method = "POST" }

maybeProps :: Maybe Props -> Endo Request
maybeProps props req = case props of
    Nothing -> req
    Just props' -> req { requestBody = RequestBodyLBS (Aeson.encode props') }

getRoot_ :: Manager -> Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot_ manager conn next = do
    req <- json . get <$> parseUrl (T.unpack (baseUrl conn))
    resp <- httpLbs req manager
    let res = Aeson.decode (responseBody resp) :: Maybe RootResponse
    maybe (return Nothing) (interpret manager conn) (next <$> res)

getNode_ :: Manager -> Connection -> Int -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
getNode_ manager conn nodeId next = do
    req <- json . get <$> parseUrl (T.unpack (singleUrl nodeId conn))
    resp <- httpLbs req manager
    let res = Aeson.decode (responseBody resp) :: Maybe NodeResponse
    maybe (return Nothing) (interpret manager conn) (next <$> res)

createNode_ :: Manager -> Connection -> Maybe Props -> (NodeResponse -> Neo4jAction a) -> IO (Maybe a)
createNode_ manager conn props next = do
    req <- json . post . maybeProps props <$> parseUrl (T.unpack (nodeUrl conn))
    resp <- httpLbs req manager
    let res = Aeson.decode (responseBody resp) :: Maybe NodeResponse
    maybe (return Nothing) (interpret manager conn) (next <$> res)

interpret :: Manager -> Connection -> Neo4jAction r -> IO (Maybe r)
interpret manager conn = \case
    Free action -> case action of
        GetRoot next -> getRoot_ manager conn next
        GetNode nodeId next -> getNode_ manager conn nodeId next
        CreateNode props next -> createNode_ manager conn props next
        _ -> undefined
    Pure r -> return (Just r)
