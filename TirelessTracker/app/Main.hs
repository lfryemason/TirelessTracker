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
main = run (AppState []) []

cmdUpdate :: AppState -> IO [Event]
-- TODO: Better show match function
-- TODO: Parse match from string function
cmdUpdate (AppState matches) = do
    putStrLn $ "Tireless Tracker: "
    command <- getLine
    if isPrefixOf "add " (map toLower command) then
        return [EAddMatch (Match {myDeck = "DeckName",
                                  oppDeck = "Unknown",
                                  result = (Win, (0, 0)),
                                  date = makeDay 2018 4 17,
                                  eventType = "Event Name"})]
    else if (map toLower command) == "show" then
        return [EShow]
    else if (map toLower command) == "exit" then
        return [EExit]
    else
        return []

run :: AppState -> [Event] -> IO ()
run (ShowMatches matches) events = do
    putStrLn $ show matches
    run (AppState matches) events

run state [] = do
    events <- cmdUpdate state
    run state events

run _ (EExit:_) =
    return ()

run state (event:events) = do
        run newState events
        where
            newState = stateUpdate state event
