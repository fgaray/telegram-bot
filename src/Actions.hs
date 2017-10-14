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
import Data.List (foldl', sortBy)
import Plots
import Data.ByteString (ByteString)
import Network.Mime
import Debug.Trace
import qualified BTFO as B



commandsHelp :: [(Text, Text)]
commandsHelp = [
      ("/dbsize", "Shows the number of total messages in the DB")
    , ("/dbstats", "Show the number of messages per user")
    , ("/markov [USER]", "")
    , ("/markovtrain [USER]", "Trains a markov chain using the messages of the [USER]")
    , ("/last [USER]", "Show the last message of the [USER]")
    , ("/help", "Shows this help")
    , ("/alltop", "Total messages of the users in the week")
    ]



processAction :: LoggerSet -> Update -> TelegramClient ()
processAction logger Update{..} =
    case message of
        Nothing -> return ()
        Just messageJust@Message{..} ->
            case text of
                Nothing -> return ()
                Just msg -> do
                    txt <- liftIO $ processTextMessage msg messageJust
                    let chatId = ChatId . chat_id $ chat
                    case txt of
                        -- | There is nothing to return
                        Nothing -> do
                            photo <- liftIO $ processPhotoMessage msg
                            case photo of
                                Nothing -> return ()
                                Just photo' -> do
                                    let upload = localFileUpload photo'
                                        rq = (uploadPhotoRequest chatId upload) {
                                            photo_caption = Just "Stats"
                                        }
                                    uploadPhotoM rq
                                    return ()
                        Just txt' -> do
                            let request = SendMessageRequest chatId txt' Nothing Nothing Nothing Nothing Nothing
                            sendMessageM request
                            return ()


-- | We call a program in awk to do simple regex and sustitution in the
-- messages, in the future we can implemente more complex funcionalities in this
-- function
processTextMessage :: Text -> Message -> IO (Maybe Text)
processTextMessage txt msg
    {-| str =~ ("(\\w|\\ )*AY(Y)+(\\w|\\ )*" :: String) = return . Just $ "LMAO"-}
    {-| str =~ ("(\\ )*(l|L)inux(\\ )*" :: String)      = return . Just $ "*GNU/Linux"-}
    {-| str =~ ("^(\\ )*(OMG|omg)(\\ )*$" :: String)    = return . Just $ "我的天啊!"-}
    | str =~ ("^/interject$" :: String)                = outputFile "interject.txt"
    | str =~ ("^/dbsize$" :: String)                   = Just <$> dbsize
    | str =~ ("^/dbstats$" :: String)                  = Just <$> dbstats
    | str =~ ("^/markovtrain (\\w)*" :: String)        = markovTrain txt
    | str =~ ("^/markov (\\w)*" :: String)             = markov txt
    | str =~ ("^/ask (\\w)*" :: String)                = cobe txt
    | str =~ ("^/train$" :: String)                    = cobeTrain
    | str =~ ("^/last (\\w)*" :: String)               = lastMsg txt
    | str =~ ("^/bestof" :: String)                    = bestof
    | str =~ ("^/topreplies" :: String)                = topreplies
    | str =~ ("^/alltop" :: String)                    = alltop
    | str =~ ("^/btfo$" :: String)                     = reportBTFO
    | str =~ ("^/help$" :: String)                     = showHelp
    | str =~ ("BTFO" :: String)                        = B.countBTFO txt msg >> return Nothing
    | otherwise                                        = return Nothing
    where
        str = unpack txt

processPhotoMessage :: Text -> IO (Maybe FilePath)
processPhotoMessage txt
    | str =~ ("^/top$" :: String)  = topLastWeek
    | otherwise                    = return Nothing
    where
        str = unpack txt


outputFile :: FilePath -> IO (Maybe Text)
outputFile = fmap Just . T.readFile


dbsize :: IO Text
dbsize = runDB $ do
    (Value val') <- fmap (fromJust . listToMaybe) . select $
        from $ \(_ :: SqlExpr (Entity UpdateMessage)) ->
        return (countRows :: SqlExpr (Value Int))
    return $ "Total messages in the database: " <> (pack . show $ val')

dbstats :: IO Text
dbstats = runDB $ do
    xs <- select $
        from $ \(m, u) -> do
        where_ (m ^. UpdateMessageFrom ==. just (u ^. UserId))
        mapM_ (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
        groupBy (just $ m ^. UpdateMessageFrom)
        return (u ^. UserFirstName, countRows :: SqlExpr (Value Int))
    let text = foldl' (\acc (name, rows) -> acc <> unValue name <> ": " <> (pack . show . unValue $ rows) <> "\n") "" xs
    return text



cobeTrain :: IO (Maybe Text)
cobeTrain = runDB $ do
    text <- fmap (mapMaybe unValue) . select $
        from $ \(m) -> do
        mapM_ (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
        return (m ^. UpdateMessageMessage)
    let ws = map unpack . map (\x -> x <> "\n") $ text
    case ws of
        [] -> return Nothing
        xs -> liftIO . fmap (Just . pack) $ readProcess "python" ["cobe.py"] (unwords xs)


cobe :: Text -> IO (Maybe Text)
cobe query = liftIO . fmap (Just . pack) $ readProcess "python" ["cobeAsk.py"] (unpack . T.unwords . drop 1 . T.words $ query)


markovTrain :: Text -> IO (Maybe Text)
markovTrain query = runDB $ do
    let elements = take 2 . T.words $ query
    if length elements /= 2 then return Nothing
        else do
            let user = elements !! 1
            text <- fmap (mapMaybe unValue) . select $
                from $ \(u, m) -> do
                where_ (just (u ^. UserId) ==. m ^. UpdateMessageFrom)
                where_ (u ^. UserFirstName `like` val ("%" <> user <> "%"))
                mapM_ (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
                return (m ^. UpdateMessageMessage)
            let ws = map unpack . map (\x -> x <> ". ") $ text
            case ws of
                [] -> return Nothing
                xs -> liftIO . fmap (Just . pack) $ readProcess "python" ["markovTrain.py", unpack user] (unwords ws)

markov :: Text -> IO (Maybe Text)
markov query = do
    let elements = take 2 . T.words $ query
    if length elements /= 2 then return Nothing
        else do
            let user = elements !! 1
            liftIO . fmap (Just . pack) $ readProcess "python" ["markov.py", unpack user] []


lastMsg :: Text -> IO (Maybe Text)
lastMsg query = runDB $ do
    liftIO $ print query
    let user = (take 2 . T.words $ query) !! 1
    last <- fmap (fmap unValue . listToMaybe) . select $
                from $ \(u, m) -> do
                where_ (just (u ^. UserId) ==. m ^. UpdateMessageFrom)
                where_ (u ^. UserFirstName `like` val ("%" <> user <> "%"))
                mapM_ (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
                orderBy [desc (m ^. UpdateMessageDate)]
                limit 1
                return (m ^. UpdateMessageMessage)
    return (join last)

showHelp :: IO (Maybe Text)
showHelp = return . Just . T.unlines . map (\(c, h) -> c <> ": " <> h) $ commandsHelp

bestof :: IO (Maybe Text)
bestof = do
    last <- lastWeek
    more <- runDB $ moreReactions last
    case more of
        Nothing -> return Nothing
        Just (u, m, v) -> 
            case  updateMessageMessage m of
                Nothing -> return . Just $ "No hay mejores comentarios esta semana"
                Just msg -> return . Just $ "Mejor comentario de la semana:\n" <> msg <> "\nPor: " <> userFirstName u <> "\nCon: " <> (T.pack . show $ v) <> " mensajes recibidos"

topreplies :: IO (Maybe Text)
topreplies = do
    last <- lastWeek
    more <- runDB $ moreReplies last
    case more of
        Nothing -> return Nothing
        Just (u, m, v) -> do
            case  updateMessageMessage m of
                Nothing -> return . Just $ "No hay mejores comentarios esta semana"
                Just msg -> return . Just $ "Mayor cantidad de respuestas de la semana:\n" <> msg <> "\nPor: " <> userFirstName u <> "\nCon: " <> (T.pack . show $ v) <> " mensajes recibidos"


alltop :: IO (Maybe Text)
alltop = do
    week <- lastWeek
    userMessages <- liftM (sortBy (\(_, m1) (_, m2) -> compare m2 m1)) . runDB $ messagesFromGroup week
    return . Just . T.unlines . map (\(u, m) -> u <> ": " <> (T.pack . show $ m)) $ userMessages
    

reportBTFO :: IO (Maybe Text)
reportBTFO = do
    btfos <- runDB $ allBTFO
    let sorted = sortBy (\(_, i1) (_, i2) -> compare i2 i1) btfos
        txt = foldl' (\acc (u, n) -> acc <> u <> ": " <> (pack . show $ n) <> "\n") "" sorted
    return (Just txt)
