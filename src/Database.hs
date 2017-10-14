{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Database where

import Database.Persist hiding (Update, (==.))
import Database.Persist.Sqlite hiding (Update, (==.))
import Database.Persist.TH
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Web.Telegram.API.Bot.Data hiding (User, from)
import qualified Web.Telegram.API.Bot.Data as Telegram
import Data.Text (Text)
import Database.Esqueleto
import qualified Database.Esqueleto as DB
import Data.Maybe
import Data.Traversable (sequence)
import Data.Serialize
import Data.Serialize.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Data.Int
import Data.List (sortBy)
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX


commands :: [Text]
commands = concat $ map (\c -> [c, "/" <> c]) cs
    where cs = 
            [ "dbsize"
            , "dbstats"
            , "markov"
            , "markovtrain"
            , "ask"
            , "train"
            , "last"
            , "help"
            , "alltop"
            ]


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UpdateMessage
    updateId Int
    messageId Int
    from UserId Maybe
    date Int
    chat Int64
    message Text Maybe
    replyTo UpdateMessageId Maybe
    deriving Show

EntityMessage
    message UpdateMessageId
    entity EntityUpdateId
    deriving Show

EntityUpdate
    typeEntity Text
    offset Int
    url Text Maybe 
    user UserId Maybe

User
    userIdTelegram Int
    userName Text Maybe
    firstName Text
    lastName Text Maybe
    deriving Show

UserBTFO
    user UserId
    btfo BTFOId

BTFO
    message Text
|]



runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB =  runSqlite "los-programadores.sqlite3"


runDBLog :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runDBLog = runStdoutLoggingT . withSqliteConn "los-programadores.sqlite3" . runSqlConn

migrate = runDB $ runMigration migrateAll


-- | Tries to insert the update in the DB. If the update does not exists,
-- returns Just Update so we can do an action with it, otherwise return Nothing
saveUpdateMessage :: Update -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe Update)
saveUpdateMessage u@Update{..} = do
    case message of
        Nothing -> return Nothing
        Just Message{..} -> do
            -- lets check if we already have that message
            previous <- liftM listToMaybe . select $
                DB.from $ \m -> do
                where_ (m ^. UpdateMessageUpdateId ==. val update_id)
                return m
            case previous of
                Just _ -> return Nothing
                Nothing -> do
                    userId <- sequence $ fmap getOrInsertUser from
                    insertedEntities <- sequence $ fmap (mapM insertEntityDB) entities
                    replyTo <- liftM join . sequence . fmap getReplyTo $ reply_to_message 
                    insert $ UpdateMessage update_id message_id userId date (chat_id chat) text replyTo
                    liftIO $ print ("Escribiendo "  ++ show update_id)
                    liftIO $ print ("Escribiendo "  ++ show message)
                    liftIO $ LBS.writeFile ("los-programadores-all/" ++ show update_id ++  ".json") (Aeson.encode u)
                    return . Just $ u




getOrInsertUser :: Telegram.User -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) UserId
getOrInsertUser Telegram.User{..} = do
    -- check if we have the user
    user <- getUserDB user_id
    case user of
        Just u -> return . entityKey $ u
        Nothing -> insert $ User user_id user_username user_first_name user_last_name

getUserDB :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Entity User))
getUserDB userId = liftM listToMaybe . select $
    from $ \u -> do
    where_ (u ^. UserUserIdTelegram ==. val userId)
    limit 1
    return u


insertEntityDB :: MessageEntity -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe EntityUpdateId)
insertEntityDB MessageEntity{..} = do
    case me_user of
        Nothing -> liftM Just . insert $ EntityUpdate me_type me_offset me_url Nothing
        Just Telegram.User{..} -> do
            user <- getUserDB user_id
            liftM Just . insert $ EntityUpdate me_type me_offset me_url (fmap entityKey user)


getReplyTo :: Message -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Key UpdateMessage))
getReplyTo Message{..} = liftM (fmap entityKey . listToMaybe) . select $
    DB.from $ \u -> do
    where_ (u ^. UpdateMessageMessageId ==. val message_id)
    limit 1
    return u
    

getLastOffset :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe Int)
getLastOffset = liftM (fmap unValue . listToMaybe) . select $
    from $ \u -> do
    orderBy [desc (u ^. UpdateMessageMessageId)]
    limit 1
    return (u ^. UpdateMessageUpdateId)


{-convertToJSON :: Int -> String -> IO ()-}
{-convertToJSON updateId path = do-}
    {-contents <- BS.readFile (path ++ show updateId ++ ".cereal")-}
    {-let decoded :: Either String Update = decode contents-}
    {-case decoded of-}
        {-Left err -> putStrLn $ "Can't decode: " ++ err-}
        {-Right x -> do-}
            {-liftIO $ LBS.writeFile (path ++ show updateId ++  ".json") (Aeson.encode x)-}



messagesFrom :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(User, UpdateMessage)]
messagesFrom x = liftM (map (\(x, y) -> (entityVal x, entityVal y))) . select $
    from $ \(u, m) -> do
    where_ (just (u ^. UserId) ==. m ^. UpdateMessageFrom)
    where_ (m ^. UpdateMessageDate DB.>. val x)
    return (u, m)


messagesFromGroup :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(Text, Int)]
messagesFromGroup day = liftM (map (\(x, y) -> (unValue x, unValue y))) . select $
    from $ \(u, m) -> do
    mapM (\x -> where_ (not_ $ m ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
    where_ (just (u ^. UserId) ==. m ^. UpdateMessageFrom)
    where_ (m ^. UpdateMessageDate DB.>. val day)
    groupBy (just $ m ^. UpdateMessageFrom)
    return (u ^. UserFirstName, countRows :: SqlExpr (Value Int))


-- | Return the message with more replies
moreReplies :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (User, UpdateMessage, Int))
moreReplies day = liftM (joinTuple . listToMaybe . map (\(z, x, y) -> (entityVal z, fmap entityVal x, unValue y))) . select $
    from $ \(user, u, r) -> do
    mapM (\x -> where_ (not_ $ u ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
    where_ (just (user ^. UserId) ==. u ^. UpdateMessageFrom)
    where_ (DB.not_ $ DB.isNothing (u ^. UpdateMessageReplyTo))
    where_ (u ^. UpdateMessageReplyTo ==. r ?. UpdateMessageId)
    where_ (u ^. UpdateMessageDate DB.>. val day)
    groupBy (just $ u ^. UpdateMessageReplyTo)
    orderBy [desc (countRows :: SqlExpr (Value Int))]
    return (user, r, countRows :: SqlExpr (Value Int))

    where
        joinTuple :: Maybe (c, Maybe a, b) -> Maybe (c, a, b)
        joinTuple Nothing = Nothing
        joinTuple (Just (_, Nothing, _)) = Nothing
        joinTuple (Just (z, Just x, y)) = Just (z, x, y)


-- | Return the message that activates the more number of reactions in a period
-- of time. We iterate over the messages in the DB and count the ones that are
-- separated by 60 seconds.
moreReactions :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (User, UpdateMessage, Int))
moreReactions day = do
    messages :: [(User, UpdateMessage, Int)] <- search
    let sorted = reverse . sortBy ((\(_, _, x) (_, _, y) -> compare x y)) $ messages
    return (listToMaybe sorted)
    where
        search = liftM (map (\(z, x, y) -> (entityVal z, entityVal x, unValue y))) . select $
            from $ \(user, u) -> do
                where_ (u ^. UpdateMessageDate DB.>. val day)
                where_ (just (user ^. UserId) ==. u ^. UpdateMessageFrom)
                mapM (\x -> where_ (not_ $ u ^. UpdateMessageMessage `like` just (val $ "%" <> x <> "%"))) commands
                return (user, u, sub_select $ from $ \other -> do
                    where_ (other ^. UpdateMessageDate DB.>. val day)
                    where_ (other ^. UpdateMessageId DB.>. u ^. UpdateMessageId)
                    where_ ((other ^. UpdateMessageDate) DB.-. u ^. UpdateMessageDate DB.<. val 60)
                    return (countRows :: SqlExpr (Value Int)))


lastWeek :: IO Int
lastWeek = do
    let lastWeek = -7*24*60*60
    today <- getCurrentTime
    return . fromTime . utcTimeToPOSIXSeconds $ lastWeek `addUTCTime` today


toTime :: Int -> UTCTime
toTime = posixSecondsToUTCTime . fromInteger . toInteger

-- | Ugly hack!
fromTime :: POSIXTime -> Int
fromTime = read . takeWhile (/='.') . show



-- | Adds a BTFO to the user un the DB
addBTFO :: [Int] -> Text -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
addBTFO [] _ = return ()
addBTFO mentions msg = do
    btfo <- insert $ BTFO msg
    forM_ mentions $ \m -> do
        user <- fmap (fmap entityKey . listToMaybe) . select $
            from $ \u -> do
            where_ (u ^. UserUserIdTelegram ==. val m)
            limit 1
            return u
        case user of
            Nothing -> return ()
            Just key -> void $ insert $ UserBTFO key btfo

addBTFOName :: [Text] -> Text -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
addBTFOName [] _ = return ()
addBTFOName mentions msg = do
    btfo <- insert $ BTFO msg
    forM_ mentions $ \m -> do
        user <- fmap (fmap entityKey . listToMaybe) . select $
            from $ \u -> do
            where_ (u ^. UserUserName ==. just (val m))
            limit 1
            return u
        case user of
            Nothing -> return ()
            Just key -> void $ insert $ UserBTFO key btfo



-- | Returns a list of all the BTFO for each user in all time
allBTFO :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [(Text, Int)]
allBTFO = fmap (map (\(u, b) -> (unValue u, unValue b))) . select $
    from $ \(u, ubtfo) -> do
    where_ (u ^. UserId ==. ubtfo ^. UserBTFOUser)
    groupBy (ubtfo ^. UserBTFOUser)
    return (u ^. UserFirstName, countRows)
