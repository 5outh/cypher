{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Cypher.Functions where

import Cypher.Types
import Cypher.Utils

import Data.Aeson
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

interpret :: Connection -> Neo4jAction r -> IO (Maybe r)
interpret conn = \case
    Free (GetRoot next) -> do
        req' <- parseUrl (T.unpack (baseUrl conn))
        
        let req = req'{
                method = "GET",
                requestHeaders = [(hContentType, "application/json")]
            }
        
        manager <- newManager defaultManagerSettings
        resp <- httpLbs req manager

        let res = decode (responseBody resp) :: Maybe RootResponse
        case res of
            Just body -> interpret conn (next body)
            _ -> return Nothing

    Free (Authenticate user pass next) -> do
        let key = authKey (encodeUtf8 user) (encodeUtf8 pass)
            url = authUrl user conn
        
        req' <- parseUrl (T.unpack url)
        
        let req = req'{
                method = "GET",
                requestHeaders = [(hContentType, "application/json"), (hAuthorization, key)]
            }
        
        manager <- newManager defaultManagerSettings
        resp <- httpLbs req manager

        let auth = decode (responseBody resp) :: Maybe AuthResponse
        case auth of 
            Just res -> interpret conn (next res)
            _ -> return Nothing

    Pure r -> return (Just r)


test user pass = do
    res <- authenticate user pass
    return ()
