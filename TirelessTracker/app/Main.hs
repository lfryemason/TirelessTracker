module Main 
(
    AppState (..),
    main
) where

import Lib
import AppState
import MatchData
import Matches
import Events
import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = run (AppState []) [ELoad "res/match.json"]

cmdUpdate :: AppState -> IO [Event]
-- TODO: Better show match function
cmdUpdate (AppState matches) = do 
    putStrLn $ "Tireless Tracker: "
    command <- getLine
    if isPrefixOf "add " (map toLower command) then
        let
            newMatch = parseMatch $ drop 4 command
        in
            eventAction (EAddMatch newMatch) (AppState matches)
    else if isPrefixOf "load " (map toLower command) then
        let
            loadFile = drop 5 command
        in
            return [ELoad loadFile] 
    else if (map toLower command) == "show" then do
        putStrLn $ showMatches matches
        return [EShow]
    else if (map toLower command) == "stats" then
        let 
            (w, l, d) = winLossDrawPerc matches
        in do
            putStrLn $ show w ++ "% won, " ++ show l ++ "% lost, " ++ show d ++ "% draw."
            return [EStats]
    else if (map toLower command) == "help" then do
        helpMessage
        return [EHelp]
    else if (map toLower command) == "exit" then
        return [EExit]
    else
        return []

run :: AppState -> [Event] -> IO ()
run state [] = do
    events <- cmdUpdate state
    run state events

run _ (EExit:_) =
    return ()

-- exception handling for wrong file
run (AppState matches) ((ELoad fileName):events) = do
    newMatches <- decodeMDs fileName
    run (AppState (matches ++ newMatches)) events

run state (event:events) = do
        run newState events
        where
            newState = stateUpdate state event
