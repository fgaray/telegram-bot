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

instance Serialize Venue
instance Serialize Contact
instance Serialize Voice
instance Serialize Video
instance Serialize Sticker
instance Serialize Animation
instance Serialize Game
instance Serialize PhotoSize
instance Serialize Document
instance Serialize Audio
instance Serialize MessageEntity
instance Serialize ChatType
instance Serialize CallbackQuery
instance Serialize ChosenInlineResult
instance Serialize Location
instance Serialize Chat
instance Serialize InlineQuery
instance Serialize Telegram.User
instance Serialize Message
instance Serialize Update


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UpdateMessage
    updateId Int
    messageId Int
    from UserId Maybe
    date Int
    chat Int
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
|]



runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite "los-programadores.sqlite3"

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
                    let serialized = encode u
                    liftIO $ print ("Escribiendo "  ++ show update_id)
                    liftIO $ BS.writeFile ("los-programadores-all/" ++ show update_id ++  ".cereal") serialized
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
