module Minesweeper.Board

import Data.Vect 
import Data.Fin
import Effects
import Effect.Random
import Effect.StdIO

-- |X,Y co-ordinate
data Pos = MkPos Nat Nat 

-- | Two positions are the same if their x and y co-oridinates
--   are equal
instance Eq Pos where 
  (MkPos x1 y1) == (MkPos x2 y2) = (x1 == x2) && (y1 == y2)


instance Show Pos where
  show (MkPos x y) = "(x : " ++ show x ++ ", y : " ++ show y ++ ")"

-- | A Square can either contain a mine or be adjacent to
--   0 - 8 mines
data SqState = Mine | Safe Nat


-- | A Square can be hidden (not yet pressed) or 
--   revealed 
data Visibility = Hidden | Revealed


data Square = MkSquare Visibility SqState


-- | Board is m * n squares
data Board : Nat -> Nat -> Type where
   MkBoard : (Vect m (Vect n Square)) -> Board m n


--instance Show (Board rows cols) where
--  show {rows} {cols} (MkBoard v) = "test"
   -- then "Empty"
   -- else "test"

strRepeat : Nat -> String -> String
strRepeat Z s = ""
strRepeat (S n) s = s ++ strRepeat n s

concatSV : Vect n String -> String
concatSV Nil = ""
concatSV (v :: vs) = v ++ (concatSV vs)

-- | Display the board, revealing all hidden squares
showRevealed : (Board rows cols) -> String
showRevealed {rows} {cols} board = if rows == 0 
  then "Empty"
  else  firstRow ++ "\n" ++ (showRows board)
 
  where firstRow = strRepeat (2 * cols + 1) "-" 
        
        showRows : (Board rows cols) -> String
        showRows (MkBoard vs) = concatSV $ map (\v => (showRow v) ++ "\n") vs

        where showRow : Vect k Square -> String
              showRow v = "|" ++ rowStr ++ "\n" ++ endStr
                where rowStr = concatSV $ map (\(MkSquare _ st) => case st of
                                    Mine => "m|"
                                    (Safe Z) => " |"
                                    (Safe n) => (show n) ++ "|" ) v 

                      -- Line under each Row
                      endStr = (concatSV $ map (\_ => "--") v) ++ "-"



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
 
-- | Given an index, element and vector create a new
--   vector with the given element at that index.
--   If the index is >= the vector size then the same
--   vector is returned 
replaceV : Nat -> a -> Vect n a -> Vect n a 
replaceV = replaceV' Z 
  where replaceV' : Nat -> Nat -> a -> Vect n a -> Vect n a 
        replaceV' _ _ _ Nil = Nil
        replaceV' cur pos a (v :: vs) = 
          if (cur == pos) 
            then a :: vs
            else v :: (replaceV' (cur + 1) pos a vs)
         

-- | Given row and column number and a Board, attempts
--   to increment the bomb adjacency value of the given
--   square in the board, if the row/col numbers are out
--   of range returns nothing, if the square selected is
--   a mine then the same Board is returned
incBoardSquare : Nat -> Nat -> (Board rows cols) -> Maybe (Board rows cols)
incBoardSquare {rows} {cols} y x (MkBoard v) = do
  rowPos <- natToFin y rows 
  colPos <- natToFin x cols
  let row  =index rowPos v
  let square = index colPos row

  let newSquare = case square of
    (MkSquare vis (Safe n)) => (MkSquare vis (Safe (S n)))
    sq => sq
    
  return $ MkBoard (replaceV y (replaceV x newSquare row) v)


-- | Attempt to increment the mine adjacency value for all 8 adjacent squares
--   of a given mine position, if the bomb position given is out of bounds then
--   Nothing is returned. Any adjacent squares which aren't on the board are 
--   not incremented
incAdjacentSquares : Pos -> (Board rows cols) -> Maybe (Board rows cols)
incAdjacentSquares {rows} {cols} (MkPos x y) board = do

    -- Increment squares to the left of the mine
    leftSide <- if x > 0 
      then do 
        left <- incBoardSquare y (x - 1) board
        topLeft <- if y > 0 then incBoardSquare (y - 1) (x - 1) left else return left
        if y < (rows - 1) then incBoardSquare (y + 1) (x - 1) topLeft else return topLeft
        
      else return board
    
    -- Increment squares to the right side of the mine
    rightSide <- if x < (cols - 1)
      then do
        right <- incBoardSquare y (x + 1) leftSide 
        topRight <- if y > 0 then incBoardSquare (y - 1) (x + 1) right else return right
        if y < (rows - 1) then incBoardSquare (y + 1) (x + 1) topRight else return topRight
      
      else return leftSide

    -- Increment squares above and below the mine
    top <- if y > 0 then incBoardSquare (y - 1) x rightSide else return rightSide
    bot <- if y < (rows - 1) then incBoardSquare (y + 1) x top else return top 

    return top --bot



-- | Given row and column  dimensions of board with mines generate 
--   the board
createBoard : (n : Nat) ->  (m : Nat) -> Vect k Pos -> Maybe (Board m n)
createBoard cols rows mines = do
      let initialBoard = MkBoard $ map (\row => generateRow row cols) (iterateV rows (+1) 0)
      calculateState initialBoard

    where generateRow : Nat -> (n : Nat) -> Vect n Square
          generateRow row cols = map (\col => generateSquare row col) (iterateV cols (+1) 0)

          where generateSquare : Nat -> Nat -> Square
                generateSquare row col = MkSquare Hidden sqState
                -- Check if Square is a mine
                where sqState = if (MkPos col row) `elem` mines 
                                then Mine 
                                else Safe 0
        
         -- For each non mine square work out the number of adjacent
         -- bombs
          calculateState : Board m n -> Maybe (Board m n)
          calculateState board = calculateState' board mines
            where calculateState' : Board m n -> Vect k Pos -> Maybe (Board m n) 
                  calculateState' board Nil = return board
                  calculateState' board (x :: xs) = do 
                    res <- (incAdjacentSquares x board) 
                    calculateState' res xs



 

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
    then return $ placeRightMine mine 
    else return mine
  
  
 where placeRightMine : Pos -> Pos
       placeRightMine (MkPos col row) = 
            if nextPos `elem` mines
              then placeRightMine nextPos 
              else nextPos
           
           -- Modulo giving strange division by 0 runtime error,
           -- so calculate next square to the right manually 
           where nextCol : Nat
                 nextCol = if (S col) == x then Z else (S col)
                 nextRow : Nat
                 nextRow = if nextCol == Z 
                              then if ((S row) == y) then Z else (S row)
                              else row
                 nextPos = (MkPos nextCol nextRow)



-- | Given x and y dimensions and number of mines to create 
--   creates a vector containing positions of randomly placed mines.
--  if there are more mines than x * y spaces then Nothing is returned.
--  To guarantee termination if a collision occurs then a mine is attempted
--  to be placed to the right of the mine until a free space is hit
generateMines' : Nat -> Nat -> (n : Nat) -> {[RND]} Eff (Vect (n) Pos)
generateMines' x y nMines = rewrite (identity_proof nMines) in 
                            (generateMines'' nMines Nil)
 
  where generateMines'' : (m : Nat) -> (Vect k Pos) -> {[RND]} Eff (Vect (m + k)  Pos)
        generateMines'' Z v = return v
        generateMines'' {k} (S l) mines = do
          mine <- generateMine x y mines 
          rewrite (plusSuccRightSucc l k) in 
            generateMines'' l (mine :: mines)     

generateMines : Nat -> Nat -> (n : Nat) -> {[RND]} Eff (Maybe (Vect n Pos))
generateMines x y nMines = 
  if (x * y < nMines) 
    then return Nothing
    else do
      mines <- generateMines' x y nMines
      return $ Just mines

        

showMines : Nat -> Nat -> Nat -> {[RND, STDIO]} Eff ()
showMines x y nMines = do
  mines <- generateMines x y nMines
  putStrLn $ show mines 


test1 : {[STDIO]} Eff ()
test1 = putStrLn "test"

main : IO()
main = do 
  let x = 30
  let y = 16
  let nMines = 99
  mines <- run (generateMines x y nMines)
  case mines of
    Nothing => putStrLn "More mines than positions available"
    Just m => do
      putStrLn $ show m
      let board = createBoard x y m
      case board of
        Nothing => putStrLn "Error creating board"
        Just b => putStrLn $ showRevealed b



