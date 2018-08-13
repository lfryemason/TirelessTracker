module Events
(
    Event (..),
    stateUpdate,
    helpMessage,
    eventAction,
    parseSortSpecifiers
) where

import AppState
import MatchData
import Matches
import Data.Char
import Data.Maybe
import Data.List

data Event =
    EAddMatch Match |
    EShow |
    ELoad String |
    ESave String |
    EStats |
    EHelp |
    ESort String |
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
        if length argMatches == 0 then
            putStrLn "No Matches Found\n"
        else
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

eventAction command (ESort empty) (AppState matches) =
    let 
        suffixCmd = drop 5 command
    in do
        return [ESort suffixCmd]
    

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


parseCommandsRes :: [String] -> (String, [( [Match] -> [Match] )] )
parseCommandsRes [] = ("", [])
parseCommandsRes (c:commands) = 
    let
        (args, resList) = parseCommandsRes commands
    in
        if c !! 0 == '-' then
            ("", commandFromString c args : resList)
        else
            if args == "" then
                (c ++ args, resList)
            else
                (c ++ " " ++ args, resList)

commandFromString :: String -> String -> ([Match] -> [Match])
commandFromString command args =
    let
        spec = commandSpecifiers command args
    in
    \matches -> filter (\m -> (spec m) == EQ) matches

commandSpecifiers :: String -> String -> (Match -> Ordering)
commandSpecifiers command args =
    if command == "-d" then
        let
            deck = Match {myDeck = args, oppDeck = "", result = (Win, (0, 0)),date = makeDay 0 0 0,eventType = ""}
        in
            (\m -> compareMatch [(\m -> D $ myDeck m )] deck m)
    else if command == "-o" then
        let
            deck = Match {myDeck = "", oppDeck = args, result = (Win, (0, 0)),date = makeDay 0 0 0,eventType = ""}
        in
            (\m -> compareMatch [(\m -> D $ oppDeck m )] deck m)
    -- else if command == "-w" then
    --     let
    --         deck = Match {myDeck = "", oppDeck = "", result = (Win, (0, 0)),date = makeDay 0 0 0,eventType = ""}
    --     in
    --         (\m -> compareMatch [(\m -> D $ myDeck m )] deck m)
    else if command == "-t" then
        if (length $ words args) == 3 then
            let
                argList = map read $ words args
                year = argList !! 0
                month = fromIntegral $ argList !! 1
                day = fromIntegral $ argList !! 2
                deck = Match {myDeck = args, oppDeck = "", result = (Win, (0, 0)),date = makeDay year month day,eventType = ""}
            in
                (\m -> compareMatch [(\m -> Dy $ date m)] deck m)
        else
            \m -> EQ
    else if command == "-e" then
        let
            deck = Match {myDeck = args, oppDeck = "", result = (Win, (0, 0)),date = makeDay 0 0 0,eventType = args}
        in
            (\m -> compareMatch [(\m -> D $ eventType m )] deck m)
    else
        \m -> EQ

parseSortSpecifiers :: String -> [Match] -> [Match]
parseSortSpecifiers command = 
    let 
        commandList = catMaybes (map sortSpecifiers $ words command)
    in
        \matches -> sortBy (compareMatch commandList) matches

sortSpecifiers :: String -> Maybe ( Match -> MatchTypes )
sortSpecifiers command = 
    if command == "-d" then
        Just (\m -> D $ myDeck m )
    else if command == "-o" then
        Just (\m -> D $ oppDeck m )
    -- else if command == "-w" then
    --     Just (\m -> D $ result m )
    else if command == "-t" then
        Just (\m -> Dy $ date m)
    else if command == "-e" then
        Just (\m -> D $ eventType m )
    else
        Nothing