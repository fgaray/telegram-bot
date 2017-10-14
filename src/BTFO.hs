{-# LANGUAGE OverloadedStrings #-}
module BTFO where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Database as DB


data Persons = Persons 
    { mentions    :: [Text]
    , accomulator ::  Maybe Text
    } deriving Show


countBTFO :: Text -> IO ()
countBTFO txt = do
    let ments = mentions . addLast . T.foldl' findPersons (Persons [] Nothing) $ txt
    DB.runDB $ DB.addBTFO ments txt

    where
        findPersons :: Persons -> Char -> Persons
        findPersons (Persons xs (Just acc)) ' ' = Persons ((T.reverse acc) : xs) Nothing
        findPersons (Persons xs (Just acc)) c   = Persons xs (Just $ T.cons c acc)
        findPersons (Persons xs Nothing) '@'    = Persons xs (Just "")
        findPersons (Persons xs Nothing) _      = Persons xs Nothing

        addLast :: Persons -> Persons
        addLast (Persons xs (Just p)) = Persons ((T.reverse p) : xs) Nothing
        addLast (Persons xs Nothing) = Persons xs Nothing

