module Minesweeper.Board

import Data.Vect 
import Data.Fin

import Effects
import Effect.Random
import Effect.StdIO
import Effect.System

-- |X,Y co-ordinate
public export
data Pos = MkPos Nat Nat 

-- | Two positions are the same if their x and y co-oridinates
--   are equal
implementation Eq Pos where 
  (MkPos x1 y1) == (MkPos x2 y2) = (x1 == x2) && (y1 == y2)

implementation Show Pos where
  show (MkPos x y) = "(x : " ++ show x ++ ", y : " ++ show y ++ ")"

-- | A Square can either contain a mine or be adjacent to
--   0 - 8 mines
data SqState = Mine | Safe Nat


-- | A Square can be hidden (not yet pressed) or 
--   revealed 
data Visibility = Hidden | Revealed


data Square = MkSquare Visibility SqState


-- | Board is m * n squares
export
data Board : Nat -> Nat -> Type where
   MkBoard : (Vect m (Vect n Square)) -> Board m n


strRepeat : Nat -> String -> String
strRepeat Z s = ""
strRepeat (S n) s = s ++ strRepeat n s

concatSV : Vect n String -> String
concatSV Nil = ""
concatSV (v :: vs) = v ++ (concatSV vs)


-- | Display the board, hiding any hidden squares
export
showBoard : (Board rows cols) -> String
showBoard {rows} {cols} board = if rows == 0 
  then "Empty"
  else  firstRow ++ "\n" ++ (showRows board)
 
  where firstRow = strRepeat (2 * cols + 1) "-" 
        
        showRows : (Board rows cols) -> String
        showRows (MkBoard vs) = concatSV $ map (\v => (showRow v) ++ "\n") vs

        where showRow : Vect k Square -> String
              showRow v = "|" ++ rowStr ++ "\n" ++ endStr
                where rowStr = concatSV $ map (\(MkSquare vis st) => case (vis, st) of
                                    (Hidden, _) => "?|"
                                    (Revealed, Mine) => "m|"
                                    (Revealed, Safe Z) => " |"
                                    (Revealed, Safe n) => (show n) ++ "|" ) v 

                      -- Line under each Row
                      endStr = (concatSV $ map (\_ => "--") v) ++ "-"


-- | Display the board, revealing all hidden squares
export
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
natPlusZ : (n : Nat) -> n = plus n 0
natPlusZ Z = Refl
natPlusZ (S o) = rewrite natPlusZ o in Refl 


-- | Apply the given function (k - 1) times to a given value and return
--   all the intermediate values as a vector
iterateV : (k : Nat) -> (a -> a) -> a -> Vect k a
iterateV k f b = rewrite (natPlusZ k) in (reverse $ iterateV' k b Nil)
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
    
  pure $ MkBoard (replaceV y (replaceV x newSquare row) v)


-- | Attempt to increment the mine adjacency value for all 8 adjacent squares
--   of a given mine position, if the bomb position given is out of bounds then
--   Nothing is returned. Any adjacent squares which aren't on the board are 
--   not incremented
incAdjacentSquares : Pos -> (Board rows cols) -> Maybe (Board rows cols)
incAdjacentSquares {rows} {cols} (MkPos x y) board = do
    -- Increment squares to the left of the mine
    leftSide <- incLeftSide board x 
    -- Increment squares to the right side of the mine
    rightSide <- incRightSide leftSide cols
    -- Increment squares above and below the mine
    top <- incTop rightSide y 
    bot <- incBot top rows 

    pure bot

  where
        incTopLeft : (Board rows cols) -> Nat -> Nat -> Maybe (Board rows cols)
        incTopLeft prev Z _ = pure prev
        incTopLeft prev _ Z = pure prev
        incTopLeft prev (S y) (S x) = incBoardSquare y x prev
        
        incBotLeft : (Board rows cols) -> Nat -> Nat -> Maybe (Board rows cols)
        incBotLeft prev Z _ = pure prev
        incBotLeft prev _ Z = pure prev
        incBotLeft prev (S r) (S x) = if y < r then incBoardSquare (y + 1) x prev else pure prev
        
        incLeftSide : (Board rows cols) -> Nat -> Maybe (Board rows cols)
        incLeftSide prev Z = pure prev
        incLeftSide prev (S x) = do
            left <- incBoardSquare y x prev
            topLeft <- incTopLeft left y (S x)
            incBotLeft topLeft rows (S x)

        incTopRight : (Board rows cols) -> Nat -> Maybe (Board rows cols)
        incTopRight prev Z = pure prev
        incTopRight prev (S y) = incBoardSquare y (x + 1) prev
       
        incBotRight : (Board rows cols) -> Nat -> Maybe (Board rows cols)
        incBotRight prev Z = pure prev
        incBotRight prev (S r) = if y < r then incBoardSquare (y + 1) (x + 1) prev else pure prev
  
        incRightSide : (Board rows cols) -> Nat -> Maybe (Board rows cols)
        incRightSide prev Z = pure prev
        incRightSide prev (S c) = if x < c
          then do
            right <- incBoardSquare y (x + 1) prev
            topRight <- incTopRight right y
            incBotRight topRight rows
          else
            pure prev

        incTop : (Board rows cols) -> Nat -> Maybe (Board rows cols)
        incTop prev Z = pure prev
        incTop prev (S y) = incBoardSquare y x prev

        incBot : Board rows cols -> Nat -> Maybe (Board rows cols)
        incBot prev Z = pure prev
        incBot prev (S r) = if y < r then incBoardSquare (y + 1) x prev else pure prev

-- | Given row and column  dimensions of board with mines generate 
--   the board
export
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
                  calculateState' board Nil = pure board
                  calculateState' board (x :: xs) = do 
                    res <- (incAdjacentSquares x board) 
                    calculateState' res xs


-- | Determines if the Board is in a "Win" State
--   i.e. all hidden squares are mines
export
checkWin : Board m n -> Bool
checkWin (MkBoard v) = foldl (\b, v => (checkRow v) && b) True v -- Check each row matches win condition
  where checkRow : Vect n Square -> Bool 
        checkRow v = foldl (\b, (MkSquare vis st) => case (vis, st) of
                      (Revealed, Safe m) => b -- All safe squares must be revealed
                      (Hidden, Mine) => b  -- Hidden mines are still a win condition
                      _ => b && False) True v

-- | Determines if the Board is in a "Lose" State
--   i.e. a revealed square is a mine
export
checkLose : Board m n -> Bool
checkLose (MkBoard v) = foldl(\b, v => (checkRow v) || b) False v
  where checkRow v = foldl (\b, (MkSquare vis st) => case (vis, st) of
                      (Revealed, Mine) => True -- If any mines are revealed, the player loses
                      _ => b || False) False v 
 

-- Reveals the given position, returns the new Board state
-- if a square is revealed, otherwise returns an error message
-- if the square has already been revealed or it's out of bounds
export
reveal : Pos -> Board rows cols -> Either String (Board rows cols)
reveal (MkPos x y) board@(MkBoard v) = do
  rowPos <- maybeToEither "Out Of Bounds." (natToFin y rows) 
  colPos <- maybeToEither "Out Of Bounds." (natToFin x cols)
  let row  =index rowPos v
  let square = index colPos row
  
 
  case square of
    (MkSquare Revealed _) => Left "Square has already been revealed"

    -- No surrounding mines, reveal all squares surrounding the revealed square
    (MkSquare Hidden (Safe Z)) => do
        let newSq = MkSquare Revealed (Safe Z)
        pure (MkBoard (replaceV y (replaceV x newSq row) v)) >>=

          \b => pure (revealTop b y) >>=
          \b => pure (revealLeft b x) >>=
          \b => pure (revealTopLeft b y x) >>=
          \b => pure (revealNoErr (MkPos x       (y + 1)) b) >>=
          \b => pure (revealNoErr (MkPos (x + 1) y      ) b) >>=
          \b => pure (revealNoErr (MkPos (x + 1) (y + 1)) b)  

    (MkSquare Hidden sq) => do 
        let newSq = MkSquare Revealed sq
        pure $ MkBoard (replaceV y (replaceV x newSq row) v)
   
 -- Reveals the given position, returns the new Board state
 -- if a square is revealed, otherwise returns the old board state
 where revealNoErr : Pos -> Board rows cols -> Board rows cols
       revealNoErr p b = case reveal p b of
                           (Right newB) => newB
                           (Left _) => b

       revealLeft : Board rows cols -> Nat -> Board rows cols
       revealLeft prev Z = prev
       revealLeft prev (S x) = revealNoErr (MkPos x (y + 1)) (revealNoErr (MkPos x y) prev)
        
       revealTop : Board rows cols -> Nat -> Board rows cols
       revealTop prev Z = prev
       revealTop prev (S y) = revealNoErr (MkPos (x + 1) y) (revealNoErr (MkPos x y) prev)

       revealTopLeft : Board rows cols -> Nat -> Nat -> Board rows cols
       revealTopLeft prev Z _ = prev
       revealTopLeft prev _ Z = prev
       revealTopLeft prev y x = revealNoErr (MkPos x y) prev

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
    then pure $ placeRightMine mine 
    else pure mine
  
  
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
generateMines' x y nMines = rewrite (natPlusZ nMines) in 
                                generateMines'' nMines Nil
 
  where generateMines'' : (m : Nat) -> (Vect k Pos) -> {[RND]} Eff (Vect (m + k)  Pos)
        generateMines'' Z v = pure v
        generateMines'' {k} (S l) mines = 
					(generateMine x y mines) >>= 
					\mine => rewrite (plusSuccRightSucc l k) in generateMines'' l (mine ::mines) 
        
export          
generateMines : Nat -> Nat -> (n : Nat) -> {[SYSTEM, RND]} Eff (Maybe (Vect n Pos))
generateMines x y nMines = 
  if (x * y < nMines) 
    then pure Nothing
    else do
      -- Use current system time as RNG seed
      t <- time 
      srand t
      pure $ Just !(generateMines' x y nMines)
      



