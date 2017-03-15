{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Actions where

import Web.Telegram.API.Bot hiding (from)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.IO.Class
import System.Process
import Control.Monad.Trans.Maybe
import Control.Monad
import System.Log.FastLogger
import Logger
import Text.Regex.TDFA
import Database
import Database.Esqueleto
import Data.Maybe
import Data.Monoid
import Data.List (foldl')


commands :: [Text]
commands = [
      "dbsize"
    , "dbstats"
    , "markov"
    , "markovtrain"
    , "ask"
    , "train"
    ]



processAction :: LoggerSet -> Update -> TelegramClient ()
processAction logger Update{..} =
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
processTextMessage txt
    | str =~ ("(\\w|\\ )*AY(Y)+(\\w|\\ )*" :: String) = return . Just $ "LMAO"
    | str =~ ("(\\ )*(l|L)inux(\\ )*" :: String)      = return . Just $ "*GNU/Linux"
    | str =~ ("^(\\ )*(OMG|omg)(\\ )*$" :: String)      = return . Just $ "我的天啊!"
    | str =~ ("^interject$" :: String)                  = outputFile "interject.txt"
    | str =~ ("^dbsize$" :: String)                     = liftM Just $ dbsize
    | str =~ ("^dbstats$" :: String)                    = liftM Just $ dbstats
    | str =~ ("^markovtrain (\\w)*" :: String)         = markovTrain txt
    | str =~ ("^markov (\\w)*" :: String)              = markov txt
    | str =~ ("^ask (\\w)*" :: String)                 = cobe txt
    | str =~ ("^train$" :: String)                      = cobeTrain
    | otherwise = return Nothing
    where
        str = unpack txt



outputFile :: FilePath -> IO (Maybe Text)
outputFile = liftM Just . T.readFile


dbsize :: IO Text
dbsize = runDB $ do
    (Value val') <- liftM (fromJust . listToMaybe) . select $
        from $ \(_ :: SqlExpr (Entity UpdateMessage)) -> do
        return (countRows :: SqlExpr (Value Int))
    return $ "Total messages in the database: " <> (pack . show $ val')

dbstats :: IO Text
dbstats = runDB $ do
    xs <- select $
        from $ \(m, u) -> do
        where_ (m ^. UpdateMessageFrom ==. just (u ^. UserId))
        mapM (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
        groupBy (just $ m ^. UpdateMessageFrom)
        return (u ^. UserFirstName, countRows :: SqlExpr (Value Int))
    let text = foldl' (\acc (name, rows) -> acc <> unValue name <> ": " <> (pack . show . unValue $ rows) <> "\n") "" xs
    return text



cobeTrain :: IO (Maybe Text)
cobeTrain = runDB $ do
    text <- liftM (catMaybes . map unValue) . select $
        from $ \(m) -> do
        mapM (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
        return (m ^. UpdateMessageMessage)
    let ws = map unpack . map (\x -> x <> "\n") $ text
    case ws of
        [] -> return Nothing
        xs -> liftIO . liftM (Just . pack) $ readProcess "python" ["cobe.py"] (unwords xs)


cobe :: Text -> IO (Maybe Text)
cobe query = liftIO . liftM (Just . pack) $ readProcess "python" ["cobeAsk.py"] (unpack . T.unwords . drop 1 . T.words $ query)


markovTrain :: Text -> IO (Maybe Text)
markovTrain query = runDB $ do
    let elements = take 2 . T.words $ query
    if length elements /= 2 then return Nothing
        else do
            let user = elements !! 1
            text <- liftM (catMaybes . map unValue) . select $
                from $ \(u, m) -> do
                where_ (just (u ^. UserId) ==. m ^. UpdateMessageFrom)
                where_ (u ^. UserFirstName `like` val ("%" <> user <> "%"))
                mapM (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
                return (m ^. UpdateMessageMessage)
            let ws = map unpack . map (\x -> x <> ". ") $ text
            case ws of
                [] -> return Nothing
                xs -> liftIO . liftM (Just . pack) $ readProcess "python" ["markovTrain.py", unpack user] (unwords ws)

markov :: Text -> IO (Maybe Text)
markov query = do
    let elements = take 2 . T.words $ query
    if length elements /= 2 then return Nothing
        else do
            let user = elements !! 1
            liftIO . liftM (Just . pack) $ readProcess "python" ["markov.py", unpack user] []
