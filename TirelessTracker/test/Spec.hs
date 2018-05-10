import MatchData
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "\nTests" [unitTests,jsonTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
        [ testCase "Match show test" $ (show exampleMatch) `compare` mStr @?= EQ
        ]
    where
        mStr = "Match {myDeck = Deck \"DeckName\", oppDeck = Deck \"Unknown\", result = (Win,0,0,0), date = 2018-04-17, eventType = \"Event Name\"}"

jsonTests :: TestTree
jsonTests = testGroup "JSON Tests" $ [
        testCase "encode/decode JSON test" $ (decode (encode exampleMatch)) `compare` Just exampleMatch @?= EQ
    ]

exampleMatch = Match {myDeck = Deck "DeckName", 
                      oppDeck = Deck "Unknown", 
                      result = (Win, 0, 0, 0),
                      date = fromGregorian 2018 4 17,
                      eventType = "Event Name"
                     }

m1 = Match { myDeck = Deck "D&T"
           , oppDeck = Deck "Test1"
           , result = (Win, 2, 0, 0)
           , date = fromGregorian 2018 5 9
           , eventType = "FNM"}

m2 = Match { myDeck = Deck "D&T"
           , oppDeck = Deck "Test2"
           , result = (Win, 2, 0, 0)
           , date = fromGregorian 2018 5 9
           , eventType = "FNM"}