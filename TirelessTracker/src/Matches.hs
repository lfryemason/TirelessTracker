{-# LANGUAGE DeriveGeneric #-}
module Matches (
    encodeMDs,
    decodeMDs,
    generateStats,
    winLossDrawPerc,
    groupMatches
) where

import Data.List
import qualified Data.ByteString.Lazy as DBL
import Data.Aeson
import MatchData
import GHC.Generics

encodeMDs :: FilePath -> [Match] -> IO ()
encodeMDs fileName lMD = do
    DBL.writeFile fileName $ encode lMD

decodeMDs :: FilePath -> IO ([Match])
decodeMDs fileName = do
    d <- (decode <$> (getJsonFromFile fileName)) :: IO (Maybe [Match])
    case d of
        Nothing -> return []
        Just ms -> return ms

getJsonFromFile :: FilePath -> IO DBL.ByteString
getJsonFromFile fileName = DBL.readFile fileName

generateStats :: FilePath -> IO(Int, Int, Int)
generateStats fileName = do
    matches <- decodeMDs fileName
    return $ winLossDrawPerc matches

winLossDrawPerc :: [Match] -> (Int, Int, Int)
winLossDrawPerc [] = (100, 0, 0)
winLossDrawPerc matches = (win, 100 - win - draw, draw)
    where
        totNum = length matches
        numWin = length $ filter (\m -> Win == fst (result m)) matches
        win = numWin `div` totNum * 100
        drawNum = length $ filter (\m -> Draw == fst (result m)) matches
        draw = drawNum `div` totNum

groupMatches :: [(Match -> MatchTypes)] -> [Match] -> [[Match]]
groupMatches fs matches = groupSortedMatches fs $ sortBy (compareMatch fs) matches

groupSortedMatches :: [(Match -> MatchTypes)] -> [Match] -> [[Match]]
groupSortedMatches _ [] = []
groupSortedMatches f (m:matches) = (m:front):(groupSortedMatches f end)
        where
            (front,end) = span (\m1 -> compareMatch f m m1 == EQ) matches