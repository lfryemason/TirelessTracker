module Main 
(
    AppState (..),
    main
) where

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
cmdUpdate state = do 
    putStrLn $ "Tireless Tracker: "
    command <- getLine
    if isPrefixOf "add " (map toLower command) then
            eventAction command (EAddMatch emptyMatch) state
    else if (map toLower command) == "show" then
        eventAction command EShow state
    else if isPrefixOf "load " (map toLower command) then
        eventAction command (ELoad "") state
    else if isPrefixOf "save" (map toLower command) then
        eventAction command (ESave "") state
    else if (map toLower command) == "stats" then
        eventAction command EStats state
    else if (map toLower command) == "help" then 
        eventAction command EHelp state
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

run (AppState matches) ((ELoad fileName):events) = do
    newMatches <- decodeMDs fileName
    run (AppState (matches ++ newMatches)) events

run (AppState matches) ((ESave fileName):events) = do
    encodeMDs fileName matches
    run (AppState matches) events

run state (event:events) = do
        run newState events
        where
            newState = stateUpdate state event
