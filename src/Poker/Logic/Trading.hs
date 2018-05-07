-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Position processing functions
-------------------------------------------------------------------------------

-- | Set first player to perform action depending on street.
setFirstPlayer :: [Player] -> Street -> [Player]
setFirstPlayer players street = 
    toggleNewActivePlayer players (getFirstPosition (length players) street)

-- | Set active player depending to given position
--   and unsets previous active player.
toggleNewActivePlayer :: [Player] -> Position -> [Player]
toggleNewActivePlayer players pos = map
    (\player -> case position player == pos of
        True  -> player { active = True  }
        False -> player { active = False })
    players    

-- | Return type of active player.
getActivePlayerType :: [Player] -> PlayerType
getActivePlayerType players = case active $ head players of
    True  -> control $ head players
    False -> getActivePlayerType $ tail players

-- | Check if active player is skippable in.
checkSkipForActivePlayer :: [Player] -> Bool
checkSkipForActivePlayer [] = False
checkSkipForActivePlayer players
    | active $ head players = case action . move $ head players of
        All_In -> True
        Folded -> True
        _      -> False
    | otherwise = checkSkipForActivePlayer $ tail players

-- | Return first position depending on amount of players and street.
getFirstPosition :: Int -> Street -> Position
getFirstPosition amountOfPlayers street = case amountOfPlayers of
    2 -> if (street == Preflop)
            then BB
            else SB
    _ -> UTG

-- | Return next position depending on amount of players and street.
getNextPositon :: Int -> Street -> Position -> Position
getNextPositon amountOfPlayers street previousPosition =
    case amountOfPlayers of
        2 -> if (street == Preflop)
                then SB
                else BB
        _ -> MP

-- | Return last position depending on amount of players and street.
getLastPosition :: Int -> Street -> Position
getLastPosition amountOfPlayers street = case amountOfPlayers of
    2 -> if (street == Preflop)
            then SB
            else BB
    _ -> BTN

-------------------------------------------------------------------------------
-- * Players actions processing functions
-------------------------------------------------------------------------------

-- | Return default move to proposed bet size when human didn't made any input.
autoHumanMove :: Player -> Int -> Move
autoHumanMove player bet
    | bet == 0 = Move Checked 0
    | bet == premadeBet = Move Checked bet
    | otherwise = Move Folded premadeBet
    where
        premadeBet = betSize $ move player

-- | Time to get response from AI player.
aiThinkTime :: Float
aiThinkTime = 1.0

-- | Time to get response from human player.
humanThinkTime :: Float
humanThinkTime = 45.0
