module Events
(
    Event (..),
    stateUpdate
) where

import AppState
import MatchData
import Matches

data Event =
    EAddMatch Match |
    EShow |
    EExit 
    deriving (Eq, Show)

stateUpdate :: AppState -> Event -> AppState
stateUpdate (AppState matches) (EAddMatch match) = AppState (match:matches)
stateUpdate (AppState matches) (EShow)           = ShowMatches matches
stateUpdate as _ = as