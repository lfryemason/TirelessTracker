{-# LANGUAGE DeriveGeneric #-}
module MatchData
( Match (..)
, MDResult (..)
, Deck
, MatchTypes (..)
, compareMatch
, newResult
) where

import Data.Time
import qualified Data.List
import Data.Aeson
import GHC.Generics
import Data.Maybe
import Test.Tasty.QuickCheck

data Match = Match { myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (MDResult, (Int, Int)),
                     date :: Day,
                     eventType :: String
                } deriving (Show, Generic, Eq)

instance Arbitrary Match where
    arbitrary = do
        m <- arbitrary
        o <- arbitrary
        w <- choose (0,2) :: Gen Int
        l <- choose (0,2) :: Gen Int
        d <- arbitrary
        e <- arbitrary
        return Match {myDeck = m, oppDeck = o, result = fromJust $ newResult (w,l), date = d, eventType = e}


instance ToJSON Match
instance FromJSON Match

data MatchTypes = D Deck | R (MDResult, (Int, Int)) | Dy Day | S String deriving (Ord, Eq)

instance Ord Match where
    compare = compareMatch [(\m -> D $ myDeck m ), (\m -> D $ oppDeck m), (\m -> R $ result m), (\m -> Dy $ date m), (\m -> S $ eventType m)]

compareMatch :: [(Match -> MatchTypes)] -> Match -> Match -> Ordering
compareMatch [] a b = EQ
compareMatch (c:cs) a b 
    | (c a) > (c b) = LT
    | (c a) < (c b) = GT
    | otherwise = compareMatch cs a b

newResult :: (Int, Int) -> Maybe (MDResult, (Int, Int))
newResult (w,l) =
    if w > 2 || l > 2 || w < 0 || l < 0 then
        Nothing 
    else if w == 2 then
        Just (Win, (w,l))
    else if l == 2 then
        Just (Loss, (w,l))
    else
        Just (Draw, (w,l))

data MDResult = Loss | Draw | Win deriving (Eq, Show, Ord, Generic, Enum, Bounded)

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