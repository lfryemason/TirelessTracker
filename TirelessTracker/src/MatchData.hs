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

data Match = Match { myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (MDResult, Int, Int, Int),
                     date :: Day,
                     eventType :: String
                } deriving (Show, Generic, Ord, Eq)

instance ToJSON Match
instance FromJSON Match

data MDResult = Win | Draw | Loss deriving (Eq, Show, Ord, Generic)
instance ToJSON MDResult
instance FromJSON MDResult

data Deck = Deck String deriving (Show, Generic, Eq, Ord)
instance ToJSON Deck
instance FromJSON Deck