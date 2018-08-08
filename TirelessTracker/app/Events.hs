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
import Data.Char

data Event =
    EAddMatch Match |
    EShow |
    ELoad String |
    ESave String |
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

eventAction command EShow (AppState matches) =
    let 
        suffixCmd = drop 5 command
        argMatches = parseCommands suffixCmd matches
    in do
        putStrLn $ showMatches argMatches
        return [EShow]

eventAction command (ELoad empty) state =
    let
        suffixCmd = drop 4 command
        fileName = dropWhile (isSpace) suffixCmd
    in
        return [ELoad fileName]

eventAction command (ESave empty) state =
    let
        suffixCmd = drop 4 command
        fileName = dropWhile (isSpace) suffixCmd
    in 
        if fileName == "" then
            return [ESave "res/match.json"]
        else 
            return [ESave fileName]

eventAction command EStats (AppState matches) =
        let 
            (w, l, d) = winLossDrawPerc matches
            numMatches = length matches
        in do
            putStrLn $ show w ++ "% won, " ++ show l ++ "% lost, " ++ show d ++ "%. " ++ show numMatches ++ " total games.\n"
            putStrLn $ oppDeckWinPercReport matches ++"\n"
            return [EStats]

eventAction command EHelp state = do
    helpMessage
    return [EHelp]

eventAction _ _ _ = return []

helpMessage :: IO ()
helpMessage = do
    helpMess <- readFile "res/HelpMessage.in"
    putStrLn helpMess

parseCommands :: String -> [Match] -> [Match]
parseCommands console matches = 
        foldr id matches parsedCommands
    where
        (str, parsedCommands) = parseCommandsRes $ words console


parseCommandsRes :: [String] -> (String, [([Match] -> [Match])])
parseCommandsRes [] = ("", [])
parseCommandsRes (c:commands) = 
    let
        (args, resList) = parseCommandsRes commands
    in
        if c !! 0 == '-' then
            ("", commandFromString c args : resList)
        else
            (c ++ args, resList)

commandFromString :: String -> String -> ([Match] -> [Match])
commandFromString command args =
    if command == "-d" then
        let
            nameDeck = Match {myDeck = args, oppDeck = "", result = (Win, (0, 0)),date = makeDay 0 0 0,eventType = ""}
            comp = (\m -> compareMatch [(\m -> D $ myDeck m )] nameDeck m == EQ)
        in
            \matches -> filter comp matches
    else
        \matches -> id matches
