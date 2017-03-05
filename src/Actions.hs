{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Actions where

import Web.Telegram.API.Bot
import Data.Text
import Control.Monad.IO.Class
import System.Process


processAction :: Update -> TelegramClient ()
processAction Update{..} =
    case message of
        Nothing -> return ()
        Just Message{..} -> do
            case text of
                Nothing -> return ()
                Just msg -> do
                    txt <- liftIO $ processTextMessage msg
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
processTextMessage :: Text -> IO (Maybe Text)
processTextMessage txt = do
    output <- readProcess "gawk" ["-f", "messages.awk"] (unpack txt)
    case output of
        "" -> return Nothing
        str -> return . Just . pack $ str
