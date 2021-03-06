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
    cmdOptions command state

cmdOptions :: String -> AppState -> IO [Event]
cmdOptions command state = 
    let
        commandLow = map toLower command
    in
        if isPrefixOf "add " commandLow then
                eventAction command (EAddMatch emptyMatch) state
        else if isPrefixOf "show" commandLow then
            eventAction command EShow state
        else if isPrefixOf "load " commandLow then
            eventAction command (ELoad "") state
        else if isPrefixOf "save" commandLow then
            eventAction command (ESave "") state
        else if isPrefixOf "sort" commandLow then
            eventAction command (ESort "") state
        else if commandLow == "stats" then
            eventAction command EStats state
        else if commandLow == "help" then 
            eventAction command EHelp state
        else if commandLow == "exit" then
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

run (AppState matches) ((ESort command):events) =
    let
        sortedMatches = parseSortSpecifiers command matches
    in do
        run (AppState sortedMatches) events

run state (event:events) = do
        run newState events
        where
            newState = stateUpdate state event
