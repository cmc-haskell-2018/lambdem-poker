-- | Core module that handles all game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen)

import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types
import Poker.Logic.Types

import Debug.Trace

-- | Launches main (table) game screen.
launchGame :: IO ()
launchGame =  do
    generator   <- getStdGen
    tableScreen <- initTableScreen generator
    resolution  <- getScreenSize
    play (display $ getMarginsFrom resolution)
      backgroundColor fps tableScreen
      drawTableScreen handleInput updateGame
    where
      display           = InWindow "Lambdem Poker" windowSize
      backgroundColor   = white
      fps               = 25

-- | Initialization of table screen.
--   All images are loaded and all player data is set.
initTableScreen :: StdGen -> IO TableScreen
initTableScreen generator = createTableScreenWith generator <$> loadedTableImages

-- | Create new table screen made of images and set default parameters.
createTableScreenWith :: StdGen -> TableImages -> TableScreen
createTableScreenWith generator imgs = TableScreen
  { state        = Dealing_Hand
  , timer        = 0.0
  , totalPlayers = 2
  , playersData  = [Player "Player 1" 1500 SB, Player "Player 2" 1500 BB]
  , handCount    = 1
  , bank         = Nothing
  , sideBank     = Nothing
  , flop         = Nothing
  , turn         = Nothing
  , river        = Nothing
  , randomizer   = generator
  , images       = imgs
  }

-- | Operate with user input.
handleInput :: Event -> TableScreen -> TableScreen
handleInput _ = id

-- | Update game status. Is used to operate with timebank. 
updateGame :: Float -> TableScreen -> TableScreen
updateGame f s 
      | timer s < 1.0 = s {timer = timer s + f}
      | otherwise     = s {state = Waiting_User_Input}
