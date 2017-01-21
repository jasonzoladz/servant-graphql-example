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
{-# LANGUAGE TypeOperators         #-}

module Application where

import           BasicPrelude               hiding (lookup)

import           Control.Monad.Trans.Except

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Control.Concurrent.STM
import           Data.Map.Strict            (lookup)

import           Data.Aeson
import           GHC.Generics
import           Servant
import           Servant.Docs
import           Servant.Mock

import           Lucid
import           Servant.HTML.Lucid

import           Characters

type MyAPI = GetCharacter :<|> Home

type GetCharacter = "character" :> Capture "character name" Text :> Get '[ JSON ] Character

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

getCharacter :: Text -> Handler Character
getCharacter character = do
  db' <- liftIO $ database
  db <- liftIO $ readTVarIO db'
  case (lookup character db) of
    Nothing -> throwE err404
    Just c  -> return c

server :: Server MyAPI
server = getCharacter :<|> homePage

mkApp :: IO Application
mkApp = return $ serve myAPI server

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
