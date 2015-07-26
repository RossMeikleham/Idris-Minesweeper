
module Minesweeper.Game

import Minesweeper.Board
import Control.Monad.State

data Difficulty = Easy | Medium | Hard

data GState = Playing | Won | Lost

-- | Get board dimensions and number of mines
--   for the given difficulty level
getSetupDetails : Difficulty -> (Pos, Nat) 
getSetupDetails Easy = ((MkPos 8 8), 10)
getSetupDetails Medium = ((MkPos 16 16), 40)
getSetupDetails Hard = ((MkPos 30 16), 99)



--data GameState = MkGameState Board GState

--Type GameState = 


-- | Reveal the square for the given position, if out
--   of bounds or already revealed returns
--   error message, otherwise returns the updated
--   GameState
--reveal : Pos -> Either String GameState
--reveal 

