
module Minesweeper

import Data.Vect as V
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
data SqState = Mine | Safe Nat


-- | A Square can be hidden (not yet pressed) or 
--   revealed 
data Visibility = Hidden | Revealed


data Square = MkSquare Visibility SqState


-- | Board is m * n squares
data Board = MkBoard (Vect m (Vect n Square))


-- | Prove n = plus n 0
identity_proof : (n : Nat) -> n = plus n 0
identity_proof Z = Refl
identity_proof (S o) = rewrite identity_proof o in Refl 


-- | Apply the given function (k - 1) times to a given value and return
--   all the intermediate values as a vector
iterateV : (k : Nat) -> (a -> a) -> a -> Vect k a
iterateV k f b = rewrite (identity_proof k) in (reverse $ iterateV' k b Nil)
  where iterateV' : (m : Nat) -> a -> (Vect n a) -> (Vect (m + n) a)
        iterateV' Z _ v = v
        iterateV' (S l) a v {n} = rewrite (plusSuccRightSucc l n) in 
                                    iterateV' l (f a) (a :: v)
   

-- | Given row and column  dimensions of board with mines generate 
--   the board
createBoard : Nat -> Nat -> Vect k Pos -> Board
createBoard rows cols mines = MkBoard $ map (\row => generateRow row cols) (iterateV rows (+1) 0)

  where generateRow : Nat -> (n : Nat) -> Vect n Square
        generateRow row cols = map (\col => generateSquare row col) (iterateV cols (+1) 0)

        where generateSquare : Nat -> Nat -> Square
              generateSquare row col = MkSquare Hidden sqState
              -- Check if Square is a mine
              where sqState = if (MkPos col row) `elem` mines 
                                then Mine 
                                else Safe 0


 





-- | Given x and y dimensions and a vector containing
--   the positions of already generated mines, places a mine
--   in a position which isn't contained in the given vector
generateMine : Nat -> Nat -> (Vect n Pos) -> {[RND]} Eff Pos
generateMine x y mines = do
  xPos <- rndInt 0 (toIntegerNat x)
  yPos <- rndInt 0 (toIntegerNat y)
  let mine = MkPos (fromIntegerNat xPos) (fromIntegerNat yPos)

  -- If position already taken then attempt to regenerate
  if mine `elem` mines 
    then generateMine x y mines
    else return mine
  
  

-- | Given x and y dimensions and number of mines to create 
--   creates a vector containing positions of randomly placed mines.
generateMines : Nat -> Nat -> (n : Nat) -> {[RND]} Eff (Vect (n) Pos)
generateMines x y nMines = rewrite (identity_proof nMines) in 
                            (generateMines' nMines Nil)

  where generateMines' : (m : Nat) -> (Vect k Pos) -> {[RND]} Eff (Vect (m + k)  Pos)
        generateMines' Z v = return v
        generateMines' {k} (S l) mines = do
          mine <- generateMine x y mines 
          rewrite (plusSuccRightSucc l k) in 
            generateMines' l (mine :: mines)     


