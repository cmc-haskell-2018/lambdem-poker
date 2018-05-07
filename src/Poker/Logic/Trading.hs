-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Position processing functions
-------------------------------------------------------------------------------

-- | Return active player.
--   Isn't safe for [] case.
getActivePlayer :: [Player] -> Player
getActivePlayer players
    | active $ head players = head players
    | otherwise = getActivePlayer (tail players)

-- | Set active player depending to given position
--   and unsets previous active player.
toggleNewActivePlayer :: [Player] -> Position -> [Player]
toggleNewActivePlayer players pos = map
    (\player -> case position player == pos of
        True  -> player { active = True  }
        False -> player { active = False })
    players    

-- | Return type of active player.
--   Isn't safe for [] case.
getActivePlayerType :: [Player] -> PlayerType
getActivePlayerType players = case active $ head players of
    True  -> control $ head players
    False -> getActivePlayerType $ tail players

-- | Check if active player is skippable.
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
getNextPositon amountOfPlayers street _ = --previousPosition =
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

-- | Return amount of players left in hand.
countInHandPlayers :: [Player] -> Int
countInHandPlayers players = foldl1 (+) (map
    (\player -> case action $ move player of
        No_Action -> 0
        Folded    -> 0
        _         -> 1)
    players)

-- | Apply move to player on given position.
applyMove :: [Player] -> Position -> Move -> [Player]
applyMove players pos mv = map
    (\player -> case position player == pos of
        True  -> player { move = mv }
        False -> player)
    players    

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
