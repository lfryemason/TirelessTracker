import MatchData
import Matches
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Data.Aeson
import Data.Maybe

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
        testCase "Encode then Decode JSON test" $ (decode (encode exampleMatch)) `compare` Just exampleMatch @?= EQ,
        testCase "Decode 1000 matches" $ do {d <- (decodeMDs "test/jTests.json"); d `compare` Just (generateMatches 1000) @?= EQ},
        jsonGoldenEncode
    ]

jsonGoldenEncode :: TestTree
jsonGoldenEncode = testGroup "Golden JSON Encoding and Decoding" [
    goldenVsFile "Encoding List UTest" "test/jTests.json" "test/jTests1.json" (encodeMDs "test/jTests1.json" (generateMatches 1000)),
    goldenVsFile "Decode then Encode 1000 matches are equal" "test/jTests.json" "test/jTests1.json" encodeDecode]

encodeDecode :: IO()
encodeDecode = do
    ml <- decodeMDs "test/jTests.json"
    case ml of
        Nothing -> writeFile "test/jTests1.json" "Fail"
        Just ms -> encodeMDs "test/jTests1.json" ms

generateMatches :: Int -> [Match]
generateMatches 0 = []
generateMatches i = Match { myDeck = Deck "D&T"
                          , oppDeck = Deck $ "Test" ++ (show i)
                          , result = (Win, 2, 0, 0)
                          , date = fromGregorian 2018 5 9
                          , eventType = "FNM"} : generateMatches (i-1)

exampleMatch = Match {myDeck = Deck "DeckName", oppDeck = Deck "Unknown", result = (Win, 0, 0, 0),date = fromGregorian 2018 4 17,eventType = "Event Name"}

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