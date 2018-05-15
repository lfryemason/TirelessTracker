module Lib
    ( someFunc
    ) where
import MatchData
import Matches

someFunc :: IO ()
someFunc = do 
    d <- generateStats "test/jTests.json"
    putStrLn $ show d
