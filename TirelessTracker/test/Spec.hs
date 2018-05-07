import MatchData
import Data.Time

main :: IO ()
main = putStrLn (show testMatches)

testMatches :: Bool
testMatches = (show m) == mStr
    where
        m = Match {matchID = 0, 
           myDeck = Deck "D&T", 
           oppDeck = Deck "Unknown", 
           result = (Win, 2, 1, 0),
           date = fromGregorian 2018 4 17,
           eventType = "TuesdayModern"
          }
        mStr = "Match {matchID = 0, myDeck = Deck \"D&T\", oppDeck = Deck \"Unknown\", result = (Win,2,1,0), date = 2018-04-17, eventType = \"TuesdayModern\"}"

