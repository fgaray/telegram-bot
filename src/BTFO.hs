{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module BTFO where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Database as DB
import Web.Telegram.API.Bot hiding (from)
import Data.Maybe




countBTFO :: Text -> Message -> IO ()
countBTFO txt Message{..} =
    case entities of
        Nothing -> return ()
        Just es -> do
            let ments = catMaybes . map (fmap user_id . me_user) $ es
            DB.runDB $ DB.addBTFO ments txt
            DB.runDB $ DB.addBTFOName (collectBTFOText txt) txt


data Persons = Persons 
    { mentions    :: [Text]
    , accomulator ::  Maybe Text
    } deriving Show


collectBTFOText :: Text -> [Text]
collectBTFOText txt = mentions . addLast . T.foldl' findPersons (Persons [] Nothing) $ txt

    where
        findPersons :: Persons -> Char -> Persons
        findPersons (Persons xs (Just acc)) ' ' = Persons ((T.reverse acc) : xs) Nothing
        findPersons (Persons xs (Just acc)) c   = Persons xs (Just $ T.cons c acc)
        findPersons (Persons xs Nothing) '@'    = Persons xs (Just "")
        findPersons (Persons xs Nothing) _      = Persons xs Nothing

        addLast :: Persons -> Persons
        addLast (Persons xs (Just p)) = Persons ((T.reverse p) : xs) Nothing
        addLast (Persons xs Nothing) = Persons xs Nothing

