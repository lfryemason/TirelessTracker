module AppState 
(
    AppState (..)
) where

import MatchData
    
data AppState = 
    AppState [Match] |
    ShowMatches [Match]
    deriving (Eq,Show)