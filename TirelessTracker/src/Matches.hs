module Matches (
    encodeMDs
) where

import qualified Data.List
import Data.Aeson
import MatchData
import GHC.Generics

encodeMDs :: String -> [Match] -> IO ()
encodeMDs fileName lMD = do
    writeFile fileName $ show $ encode lMD