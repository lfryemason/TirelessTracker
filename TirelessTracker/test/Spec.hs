import MatchData
import Matches
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Tasty.QuickCheck as QC
import Data.Aeson
import Data.Maybe
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "\nTests" [unitTests,jsonTests, statTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
        [ testCase "Match show test" $ (show exampleMatch) `compare` mStr @?= EQ
        ]
    where
        mStr = "Match {myDeck = \"DeckName\", oppDeck = \"Unknown\", result = (Win,(0,0)), date = 2018-04-17, eventType = \"Event Name\"}"

statTests :: TestTree
statTests = testGroup "Generating Statistics Tests" $ [
        QC.testProperty "Win Draw and Loss percentages == 100" $ \matches -> let (w,l,d) = winLossDrawPerc (matches :: [Match]) in w + l + d == 100,
        testCase "Comparing by myDeck" $ compareMatch [(\m -> D $ myDeck m )] m1 m2 @?= GT,
        testCase "Comparing by result" $ compareMatch [(\m -> R $ result m )] m1 m2 @?= EQ,
        testCase "Comparing by date" $ compareMatch [(\m -> Dy $ date m )] m1 m2 @?= LT,
        testGroupMatches
        ]

testGroupMatches :: TestTree
testGroupMatches = testGroup "Grouping by opponent deck" [
        QC.testProperty "Random list of replicated set gives same size" $ testGroupMatches1,
        QC.testProperty "Random repeated list of set gives same size" $ testGroupMatches2
    ]

testGroupMatches1 :: Set.Set Deck -> Positive Int -> Bool
testGroupMatches1 sMatch i = 
    let 
        repMatches = Set.foldr (\d a-> (replicate (getPositive i) (exampleOppDeck d)) ++ a) [] sMatch
    in
        length (groupMatches repMatches) == Set.size sMatch

testGroupMatches2 :: Set.Set Deck -> Positive Int -> Bool
testGroupMatches2 sMatch i = 
    let 
        matches = Set.foldr (\d a-> (exampleOppDeck d):a) [] sMatch
        repMatches = concat $ replicate (getPositive i) matches
    in
        length (groupMatches repMatches) == Set.size sMatch

exampleOppDeck :: Deck -> Match
exampleOppDeck d = Match { myDeck = "Test1"
                         , oppDeck = d
                         , result = (Win, (2, 0))
                         , date = fromGregorian 2018 5 9
                         , eventType = "FNM"}

jsonTests :: TestTree
jsonTests = testGroup "JSON Tests" $ [
        testCase "Encode then Decode JSON test" $ (decode (encode exampleMatch)) `compare` Just exampleMatch @?= EQ,
        testCase "Decode 1000 matches" $ do {d <- (decodeMDs "test/jTests.json"); d `compare` generateMatches 1000 @?= EQ},
        jsonGoldenEncode
    ]

jsonGoldenEncode :: TestTree
jsonGoldenEncode = testGroup "Golden JSON Encoding and Decoding" [
    goldenVsFile "Encoding List UTest" "test/jTests.json" "test/jTests1.json" (encodeMDs "test/jTests1.json" (generateMatches 1000)),
    goldenVsFile "Decode then Encode 1000 matches are equal" "test/jTests.json" "test/jTests1.json" encodeDecode]

encodeDecode :: IO()
encodeDecode = do
    ml <- decodeMDs "test/jTests.json"
    encodeMDs "test/jTests1.json" ml

generateMatches :: Int -> [Match]
generateMatches 0 = []
generateMatches i = Match { myDeck = "D&T"
                          , oppDeck = "Test" ++ (show i)
                          , result = (Win, (2, 0))
                          , date = fromGregorian 2018 5 9
                          , eventType = "FNM"} : generateMatches (i-1)

exampleMatch = Match {myDeck = "DeckName", oppDeck = "Unknown", result = (Win, (0, 0)),date = fromGregorian 2018 4 17,eventType = "Event Name"}

m1 = Match { myDeck = "Test1"
           , oppDeck = "Test1"
           , result = (Win, (2, 0))
           , date = fromGregorian 2018 5 9
           , eventType = "FNM"}

m2 = Match { myDeck = "Test2"
           , oppDeck = "Test2"
           , result = (Win, (2, 0))
           , date = fromGregorian 2013 5 9
           , eventType = "FNM"}
