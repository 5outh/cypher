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

authenticate :: T.Text -> T.Text -> Neo4jAction AuthResponse
authenticate user pass = liftF (Authenticate user pass id)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id 

addHeader :: Header -> Endo Request
addHeader header req = req { requestHeaders = header: requestHeaders req }

json :: Endo Request
json = addHeader (hContentType, "application/json")

get :: Endo Request
get req = req { method = "GET" }

getRoot :: Manager -> Connection -> (RootResponse -> Neo4jAction a) -> IO (Maybe a)
getRoot manager conn next = do
    req <- json . get <$> parseUrl (T.unpack (baseUrl conn))
    resp <- httpLbs req manager
    let res = Aeson.decode (responseBody resp) :: Maybe RootResponse
    maybe (return Nothing) (interpret manager conn) (next <$> res) 

interpret :: Manager -> Connection -> Neo4jAction r -> IO (Maybe r)
interpret manager conn = \case
    Free action -> case action of
        GetRoot next -> getRoot manager conn next
        _ -> undefined
    Pure r -> return (Just r)


test user pass = do
    res <- authenticate user pass
    return ()
