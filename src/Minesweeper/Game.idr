
module Minesweeper.Game

import Minesweeper.Board
import Control.Monad.State

public export
data Difficulty = Easy | Medium | Hard

public export
data GState = Playing String | Won | Lost

-- | Get board dimensions and number of mines
--   for the given difficulty level
export
getSetupDetails : Difficulty -> (Pos, Nat) 
getSetupDetails Easy = ((MkPos 8 8), 10)
getSetupDetails Medium = ((MkPos 16 16), 40)
getSetupDetails Hard = ((MkPos 30 16), 99)

-- | Attempt to reveal the given position
-- returns the GameState after this move
export
revealPos : Pos -> State (Board m n) GState
revealPos pos@(MkPos x y) = do
  board <- get
  case (reveal pos board) of
    (Left err) => pure $ Playing err
    (Right newBoard) => do
      put newBoard
      if checkWin newBoard then pure Won
      else if checkLose newBoard then pure Lost
      else pure $ Playing $ "Revealed square in row:" ++ show y ++ " col:" ++  show x ++ "\n"
