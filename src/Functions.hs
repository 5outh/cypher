{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils

import qualified Data.Aeson as Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Control.Monad.Free
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Control.Applicative
import Data.Text.Encoding

authenticate :: T.Text -> T.Text -> Neo4jAction AuthResponse
authenticate user pass = liftF (Authenticate user pass id)

root :: Neo4jAction RootResponse
root = liftF $ GetRoot id 

addHeader header req = req { requestHeaders = header: requestHeaders req }
json = addHeader (hContentType, "application/json")
get req = req { method = "GET" }
auth key = addHeader (hAuthorization, key)

-- TODO: Clean
interpret :: Manager -> Connection -> Neo4jAction r -> IO (Maybe r)
interpret manager conn = \case
    Free action -> case action of

        GetRoot next -> do

            req <- json . get <$> parseUrl (T.unpack (baseUrl conn))
            resp <- httpLbs req manager

            let res = Aeson.decode (responseBody resp) :: Maybe RootResponse
            
            case res of
                Just body -> interpret manager conn (next body)
                _ -> return Nothing

        Authenticate user pass next -> do
            let key = authKey (encodeUtf8 user) (encodeUtf8 pass)
                url = authUrl user conn
            
            req <- json . get . auth key <$> parseUrl (T.unpack url)

            resp <- httpLbs req manager

            let auth = Aeson.decode (responseBody resp) :: Maybe AuthResponse
            case auth of 
                Just res -> interpret manager conn (next res)
                _ -> return Nothing

    Pure r -> return (Just r)


test user pass = do
    res <- authenticate user pass
    return ()
