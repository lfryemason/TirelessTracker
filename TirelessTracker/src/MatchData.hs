module MatchData
( Match (..)
, Result (..)
, Deck (..)
) where

import Data.Time
import qualified Data.List

data Match = Match { matchID :: Int,
                     myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (Result, Int, Int, Int),
                     date :: Day,
                     eventType :: String
                } deriving (Show)


data Result = Win | Draw | Loss deriving (Eq, Show, Ord)

data Deck = Deck String deriving (Show)


