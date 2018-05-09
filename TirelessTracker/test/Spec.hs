import MatchData
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
        [ testCase "Match show test" $ (show exampleMatch) `compare` mStr @?= EQ
        , testCase "encode/decode JSON test" $ (decode (encode exampleMatch)) `compare` Just exampleMatch @?= EQ
        ]
    where
        mStr = "Match {myDeck = Deck \"DeckName\", oppDeck = Deck \"Unknown\", result = (Win,0,0,0), date = 2018-04-17, eventType = \"Event Name\"}"


exampleMatch = Match {myDeck = Deck "DeckName", 
                      oppDeck = Deck "Unknown", 
                      result = (Win, 0, 0, 0),
                      date = fromGregorian 2018 4 17,
                      eventType = "Event Name"
                     }