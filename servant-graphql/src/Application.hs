{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Application where

import           BasicPrelude

import           Control.Monad.Trans.Except

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Control.Concurrent.STM
import qualified Data.Map.Strict                  as M

import           Data.Aeson
import           GHC.Generics
import           Servant
import           Servant.API.Experimental.Auth
import           Servant.Docs
import           Servant.Mock
import           Servant.Server.Experimental.Auth

import           Lucid
import           Servant.HTML.Lucid

import           Data.GraphQL

import           Characters
import           GraphQLCharacters

type instance AuthServerData (AuthProtect "x-auth-token") = ByteString -- in a real application you'd return a User

authHandler :: AuthHandler Request ByteString -- same comment as above
authHandler = mkAuthHandler handler
  where
    handler req =
      case lookup "x-auth-token" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" } )
        Just token -> if token == "open sesame" -- in a real application you would validate the token
                      then return token
                      else throwError (err401 { errBody = "Incorrect token" } )

genAuthServerContext :: Context (AuthHandler Request ByteString ': '[]) -- create a context with your AuthHandler Request User
genAuthServerContext = authHandler :. EmptyContext

type MyAPI = GetCharacter :<|> PostGraphQL :<|> Home

-- Here we protect the character json api with token auth
type GetCharacter = "character" :> Capture "character name" Text :> AuthProtect "x-auth-token" :> Get '[ JSON ] Character

type PostGraphQL = "graphql" :> AuthProtect "x-auth-token" :> ReqBody '[PlainText] Text :> Post '[JSON] Value
-- No Authentication required here
type Home = Get '[HTML] (Html ())

homePage :: Handler (Html ())
homePage = return $ do
  html_ $ do
    head_ $
      title_ "Welcome to Servant!"
    body_ $
      h1_ "Welcome to GraphQL and Servant!"

myAPI :: Proxy MyAPI
myAPI = Proxy

getCharacter :: Text -> ByteString -> Handler Character
getCharacter character _ = do
  db' <- liftIO $ database
  db <- liftIO $ readTVarIO db'
  case (M.lookup character db) of
    Nothing -> throwE err404
    Just c  -> return c

postGraphQL :: ByteString -> Text -> Handler Value
postGraphQL _ query = do
  case graphql schema query of
    Nothing  -> throwE (err400 { errBody = "Invalid graphql query" } )
    Just val -> return val

server :: Server MyAPI
server = getCharacter :<|> postGraphQL :<|> homePage

mkApp :: IO Application
mkApp = return $ serveWithContext myAPI genAuthServerContext server

run :: IO ()
run = do
  let port = 4444
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn "Servant is up!") $
        defaultSettings
  runSettings settings =<< mkApp

{-
apiDocs :: String
apiDocs = markdown $ docs myAPI

mockServer :: IO ()
mockServer = do
  putStrLn "Starting mockserver"
  run 8080 $ serve myAPI $ mock myAPI Proxy
-}
