module AppState 
(
    AppState (..)
) where

import MatchData
    
data AppState = 
    AppState [Match]
    deriving (Eq,Show)