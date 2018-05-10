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

decodeMDs :: String -> IO (Maybe [Match])
decodeMDs fileName = do
    (decode <$> getJsonFromFile fileName) :: IO(Maybe [Match])

getJsonFromFile :: String -> IO DBL.ByteString
getJsonFromFile fileName = DBL.readFile fileName