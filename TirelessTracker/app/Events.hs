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
    ELoad String |
    EStats |
    EHelp |
    EExit 
    deriving (Eq, Show)

stateUpdate :: AppState -> Event -> AppState
stateUpdate (AppState matches) (EAddMatch match) = AppState (match:matches)
stateUpdate as _ = as

eventAction :: String -> Event -> AppState -> IO [Event]
eventAction command (EAddMatch newMatches) state =
    let
        newMatch = parseMatch $ drop 4 command
    in
        case newMatch of
            Nothing -> do
                putStrLn "Invalid match, use the following format:\n[deck name] [opponent deck name] [# of won games] [# of lost games] [year] [month as number] [day of month] [event name]\n"
                return []
            Just match -> return [EAddMatch match]

eventAction command EShow (AppState matches) = do
    putStrLn $ showMatches matches
    return [EShow]

eventAction command (ELoad fileName) state =
    let
        loadFile = drop 5 command
    in
        return [ELoad loadFile] 

eventAction command EStats (AppState matches) =
        let 
            (w, l, d) = winLossDrawPerc matches
        in do
            putStrLn $ show w ++ "% won, " ++ show l ++ "% lost, " ++ show d ++ "% draw.\n"
            return [EStats]

eventAction command EHelp state = do
    helpMessage
    return [EHelp]

eventAction _ _ _ = return []


helpMessage :: IO ()
helpMessage = do
    helpMess <- readFile "res/HelpMessage.in"
    putStrLn helpMess