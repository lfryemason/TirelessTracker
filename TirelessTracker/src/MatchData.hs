{-# LANGUAGE DeriveGeneric #-}
module MatchData
( Match (..)
, MDResult (..)
, Deck
) where

import Data.Time
import qualified Data.List
import Data.Aeson
import GHC.Generics
import Test.Tasty.QuickCheck

data Match = Match { myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (MDResult, (Int, Int)),
                     date :: Day,
                     eventType :: String
                } deriving (Show, Generic, Ord, Eq)

instance Arbitrary Match where
    arbitrary = do
        m <- arbitrary
        o <- arbitrary
        r <- arbitrary
        w <- choose (0,2) :: Gen Int
        l <- choose (0,2) :: Gen Int
        d <- arbitrary
        e <- arbitrary
        return Match {myDeck = m, oppDeck = o, result = r, date = d, eventType = e}


instance ToJSON Match
instance FromJSON Match

data MDResult = Win | Draw | Loss deriving (Eq, Show, Ord, Generic, Enum, Bounded)

instance Arbitrary MDResult where
    arbitrary = do arbitraryBoundedEnum

instance ToJSON MDResult
instance FromJSON MDResult

type Deck = String 

instance Arbitrary Day where
    arbitrary = do
        y <- choose (1950,2050)
        m <- choose (1,12)
        d <- choose (1,31)
        return $ fromGregorian y m d