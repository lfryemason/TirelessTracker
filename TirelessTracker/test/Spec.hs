import MatchData
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
        [testCase "Match show test" $ (show m) `compare` mStr @?= EQ ]
    where
        m = Match {matchID = 0, 
           myDeck = Deck "DeckName", 
           oppDeck = Deck "Unknown", 
           result = (Win, 0, 0, 0),
           date = fromGregorian 2018 4 17,
           eventType = "Event Name"
          }
        mStr = "Match {matchID = 0, myDeck = Deck \"DeckName\", oppDeck = Deck \"Unknown\", result = (Win,0,0,0), date = 2018-04-17, eventType = \"Event Name\"}"

