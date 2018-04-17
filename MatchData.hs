import Data.Time
import qualified Data.List as L

data Match = Match { matchID :: Int,
					 myDeck :: Deck,
					 oppDeck :: Deck,
					 result :: (Result, Int, Int, Int),
					 date :: Day,
					 eventType :: String
				   } deriving (Show)
					 

data Result = Win | Draw | Loss deriving (Eq, Show, Ord)

data Deck = Deck String deriving (Show)

m = Match {matchID = 0, 
           myDeck = Deck "D&T", 
           oppDeck = Deck "Unknown", 
           result = (Win, 2, 1, 0),
           date = fromGregorian 2018 4 17,
           eventType = "TuesdayModern"
          }

