{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Web.Telegram.API.Bot
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Database
import Actions
import Data.Maybe


token :: Token
token = Token "bot"



main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    migrate
    runClient client token manager
    return ()

client :: TelegramClient ()
client = forever $ do
    updates <- liftM result $ getUpdatesM
    newUpdates <- liftM catMaybes . liftIO $ runDB (mapM saveUpdateMessage updates)
    mapM processAction newUpdates
    liftIO $ threadDelay 3000000
