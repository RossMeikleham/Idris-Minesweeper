
module Minesweeper.Game

import Minesweeper.Board
import Control.Monad.State

data Difficulty = Easy | Medium | Hard

data GState = Playing String | Won | Lost

-- | Get board dimensions and number of mines
--   for the given difficulty level
getSetupDetails : Difficulty -> (Pos, Nat) 
getSetupDetails Easy = ((MkPos 8 8), 10)
getSetupDetails Medium = ((MkPos 16 16), 40)
getSetupDetails Hard = ((MkPos 30 16), 99)


GameState : Nat -> Nat -> Type
GameState m n = State (Board m n) GState


-- | Attempt to reveal the given position
-- returns the GameState after this move
reveal : Pos -> GameState m n
reveal pos@(MkPos x y) = do
  board <- get
  case (reveal pos board) of
    (Left err) => return $ Playing err
    (Right newBoard) => do
      put newBoard
      if checkWin newBoard then return Won
      else if checkLose newBoard then return Lost
      else return $ Playing $ "Revealed square in row:" ++ show y ++ " col:" ++  show x ++ "\n"
