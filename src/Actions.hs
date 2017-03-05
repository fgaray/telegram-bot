{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Actions where

import Web.Telegram.API.Bot
import Data.Text
import Control.Monad.IO.Class
import System.Process
import Control.Monad.Trans.Maybe
import System.Log.FastLogger
import Logger


processAction :: LoggerSet -> Update -> TelegramClient ()
processAction logger Update{..} =
    case message of
        Nothing -> return ()
        Just Message{..} -> do
            case text of
                Nothing -> return ()
                Just msg -> do
                    txt <- liftIO $ processTextMessage logger msg
                    case txt of
                        -- | There is nothing to return
                        Nothing -> return ()
                        Just txt' -> do
                            let chatId = pack . show . chat_id $ chat
                                request = SendMessageRequest chatId txt' Nothing Nothing Nothing Nothing Nothing
                            response <- sendMessageM request
                            return ()


-- | We call a program in awk to do simple regex and sustitution in the
-- messages, in the future we can implemente more complex funcionalities in this
-- function
processTextMessage :: LoggerSet -> Text -> IO (Maybe Text)
processTextMessage logger txt = do
    pushLogStrLn logger "Calling external process"
    output <- readProcess "gawk" ["-f", "messages.awk"] (unpack txt)
    pushLogStrLn logger "Process finishied"
    case output of
        "" -> return Nothing
        str -> return . Just . pack $ str


