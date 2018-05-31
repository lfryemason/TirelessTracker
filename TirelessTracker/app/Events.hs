module Events
(
    Event (..),
    stateUpdate,
    helpMessage
) where

import AppState
import MatchData
import Matches

data Event =
    EAddMatch Match |
    EShow |
    EHelp |
    EExit 
    deriving (Eq, Show)

stateUpdate :: AppState -> Event -> AppState
stateUpdate (AppState matches) (EAddMatch match) = AppState (match:matches)
stateUpdate as _ = as

helpMessage :: IO ()
helpMessage = do
    helpMess <- readFile "src/HelpMessage.in"
    putStrLn helpMess