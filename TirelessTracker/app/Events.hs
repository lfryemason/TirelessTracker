module Events
(
    Event (..),
    stateUpdate,
    helpMessage,
    eventAction
) where

import AppState
import MatchData
import Matches

data Event =
    EAddMatch Match |
    EShow |
    EHelp |
    ELoad String |
    EStats |
    EExit 
    deriving (Eq, Show)

stateUpdate :: AppState -> Event -> AppState
stateUpdate (AppState matches) (EAddMatch match) = AppState (match:matches)
stateUpdate as _ = as

eventAction :: Event -> AppState -> IO [Event]
eventAction (EAddMatch newMatch) (AppState matches) =
    case newMatch of
        Nothing -> return []
        Just match -> return [EAddMatch match]
--eventAction EShow (Appstate matches) =
--eventAction EHelp (Appstate matches) =
--eventAction (ELoad fileName (Appstate matches) =
--eventAction EStats (Appstate matches) =
--eventAction EExit (Appstate matches) =
eventAction _ _ = return []
helpMessage :: IO ()
helpMessage = do
    helpMess <- readFile "res/HelpMessage.in"
    putStrLn helpMess