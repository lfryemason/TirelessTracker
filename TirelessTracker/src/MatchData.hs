{-# LANGUAGE DeriveGeneric #-}
module MatchData
( Match (..)
, MDResult (..)
, Deck (..)
) where


import Data.Time
import qualified Data.List
import Data.Aeson
import GHC.Generics
import Data.Text

data Match = Match { myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (MDResult, Int, Int, Int),
                     date :: Day,
                     eventType :: String
                } deriving (Show, Generic, Ord, Eq)

instance ToJSON Match
instance FromJSON Match
    {-
    toJSON (Match myDeck oppDeck result date eventType) = object [
        "myDeck"    .= myDeck,
        "oppDeck"   .= oppDeck,
        "result"    .= result,
        "date"      .= date,
        "eventType" .= eventType ]-}


exampleMatch = Match {myDeck = Deck (pack "DeckName"), 
oppDeck = Deck (pack "Unknown"), 
result = (Win, 0, 0, 0),
date = fromGregorian 2018 4 17,
eventType = "Event Name"
}
data MDResult = Win | Draw | Loss deriving (Eq, Show, Ord, Generic)
instance ToJSON MDResult
instance FromJSON MDResult

data Deck = Deck Text deriving (Show, Generic, Eq, Ord)
instance ToJSON Deck
instance FromJSON Deck