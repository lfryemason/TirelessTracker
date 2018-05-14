{-# LANGUAGE DeriveGeneric #-}
module Matches (
    encodeMDs,
    decodeMDs
) where

import qualified Data.List
import qualified Data.ByteString.Lazy as DBL
import Data.Aeson
import MatchData
import GHC.Generics

encodeMDs :: String -> [Match] -> IO ()
encodeMDs fileName lMD = do
    DBL.writeFile fileName $ encode lMD

decodeMDs :: String -> IO ([Match])
decodeMDs fileName = do
    d <- (decode <$> (getJsonFromFile fileName)) :: IO (Maybe [Match])
    case d of
        Nothing -> return []
        Just ms -> return ms

getJsonFromFile :: String -> IO DBL.ByteString
getJsonFromFile fileName = DBL.readFile fileName
