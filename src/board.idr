
module Minesweeper

import Data.Vect
import Effects
import Effect.Random

-- |X,Y co-ordinate
data Pos = MkPos Nat Nat 

-- | Two positions are the same if their x and y co-oridinates
--   are equal
instance Eq Pos where 
  (MkPos x1 y1) == (MkPos x2 y2) = (x1 == x2) && (y1 == y2)


-- | A Square can either contain a mine or be adjacent to
--   0 - 8 mines
data Contain = Mine | Safe Nat


-- | A Square can be hidden (not yet pressed) or 
--   revealed 
data Visibility = Hidden | Revealed


data Square = MkState Visibility Contain 


--record Board where
--  constructor MkBoard


--data Board =



 

-- | Given x and y dimensions and a vector containing
--   the positions of already generated mines, places a mine
--   in a position which isn't contained in the given vector
generateMine : Nat -> Nat -> Vect n Pos -> {[RND]} Eff (Vect (n + 1) Pos)
generateMine x y mines = do
  xPos <- rndInt 0 (toIntegerNat x)
  yPos <- rndInt 0 (toIntegerNat y)
  let mine = MkPos (fromIntegerNat xPos) (fromIntegerNat yPos)

  -- If position already taken then attempt to regenerate
  if mine `elem` mines 
    then generateMine x y mines
    else return (mines ++ [mine])
  
  

-- | Given x and y dimensions and number of mines to create 
--   creates a vector containing positions of randomly placed mines.
generateMines : Nat -> Nat -> Nat -> {[RND]} Eff (Vect n Pos)
generateMines x y nMines = iterateM nMines Effects.Env.Nil

  where iterateM : (m : Nat) -> Vect n Pos -> {[RND]} Eff (Vect (n + m) Pos)
        iterateM Z v = return (v ++ Nil)
        iterateM m v = do
          mines <- generateMine x y v 
          iterateM (m - 1) mines
          






