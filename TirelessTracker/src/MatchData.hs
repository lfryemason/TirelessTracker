{-# LANGUAGE DeriveGeneric #-}
module MatchData
(
    Match (..),
    MDResult (..),
    Deck,
    MatchTypes (..),
    compareMatch,
    newResult,
    makeDay,
    parseMatch
) where

import Data.Time
import qualified Data.List
import Data.Aeson
import GHC.Generics
import Data.Maybe
import Test.Tasty.QuickCheck
import Text.Read


--TODO: Add format
data Match = Match { myDeck :: Deck,
                     oppDeck :: Deck,
                     result :: (MDResult, (Int, Int)),
                     date :: Day,
                     eventType :: String
                } deriving (Generic, Eq)

instance Arbitrary Match where
    arbitrary = do
        m <- arbitrary
        o <- arbitrary
        w <- choose (0,2) :: Gen Int
        l <- choose (0,2) :: Gen Int
        d <- arbitrary
        e <- arbitrary
        return Match {myDeck = m, oppDeck = o, result = fromJust $ newResult (w,l), date = d, eventType = e}

instance Show Match where
    --show :: Match -> String
    show match = 
            deck ++ " vs " ++ opp ++ ": " ++ res ++ ". " ++ day ++ " at " ++ event
        where
            deck = myDeck match
            opp = oppDeck match
            (mRes, games) = result match
            res = show mRes ++ " " ++ show games
            day = show $ date match
            event = eventType match

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

makeDay :: Integer -> Int -> Int -> Day
makeDay = fromGregorian

parseMatch :: String -> Maybe Match
parseMatch matchStr =
    let
        matchArgs = words matchStr
    in
        if (length matchArgs) /= 8 then
            Nothing
        else if (readMaybe (matchArgs !! 2) ::Maybe Int) == Nothing || 
                (readMaybe (matchArgs !! 3) ::Maybe Int) == Nothing then
            Nothing
        else if (readMaybe (matchArgs !! 4) ::Maybe Integer) == Nothing || 
                (readMaybe (matchArgs !! 5) ::Maybe Int) == Nothing || 
                (readMaybe (matchArgs !! 6) ::Maybe Int) == Nothing then
            Nothing
        else
            let
                myD = matchArgs !! 0
                oppD = matchArgs !! 1
                w = read (matchArgs !! 2) ::Int
                l = read (matchArgs !! 3) ::Int
                res = newResult (w,l)
                year = read (matchArgs !! 4) ::Integer
                month = read (matchArgs !! 5) ::Int
                day = read (matchArgs !! 6) ::Int
                event = matchArgs !! 7
            in
            case res of
                Nothing -> Nothing
                Just res -> Just Match { myDeck = myD
                , oppDeck = oppD
                , result = res
                , date = makeDay year month day
                , eventType = event}