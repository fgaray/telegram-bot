{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Server (server) where

import Web.Telegram.API.Bot hiding (from)
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Aeson (decode)
import Data.Word (Word8)
import Logger
import Network.HTTP.Client (Manager)
import Plots

instance FromHttpApiData ChatId where
    -- naive and inefficient implementation
    parseUrlPiece = Right . ChatId . read . T.unpack
    
instance FromHttpApiData Action where
    -- naive and inefficient implementation
    parseUrlPiece = Right . read . T.unpack

data Action =
      Top
    | Friday
    deriving Read

--------------------------------------------------------------------------------

type API = "bot" :> Capture "action" Action :> Capture "chat" ChatId :> Post '[JSON] ()


server :: Token -> Manager -> IO ()
server token manager = run 8088 (serverApi token manager)

serverApi :: Token -> Manager -> Application
serverApi token manager = serve (Proxy :: Proxy API) (app token manager)

app :: Token -> Manager -> Server API
app token manager = (sendTop token manager)

sendTop :: Token -> Manager -> Server API
sendTop token manager action chatId = do
    liftIO $ runClient (client action chatId) token manager
    return ()



client :: Action -> ChatId -> TelegramClient ()
client Top chatId = do
    plot <- liftIO topLastWeek
    case plot of
        Nothing -> return ()
        Just plot' -> do
            let upload = localFileUpload plot'
                rq = (uploadPhotoRequest chatId upload) { photo_caption = Just "Stats" }
            uploadPhotoM rq
            return ()
client Friday chatId = do
    let upload = localFileUpload "friday/hqdefault.jpg"
        rq = (uploadPhotoRequest chatId upload) { photo_caption = Just "Friday!" }
    uploadPhotoM rq
    let request = SendMessageRequest chatId (T.pack . unlines $ txt) Nothing Nothing Nothing Nothing Nothing
    sendMessageM request
    return ()
    where
        txt = [
              "1) ¿Qué van a jugar?"
            , "2) ¿Qué van a ver?"
            , "3) ¿Qué van a escuchar?"
            , "4) ¿Qué van a comer?"
            ]
        
        
