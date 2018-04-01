
-- | REPL for console version of Minesweeper
module Minesweeper.REPL

import Minesweeper.Game
import Minesweeper.Board
import Minesweeper.Helper

import Control.Monad.State

import Data.Vect 

import Effects
import Effect.Random
import Effect.StdIO
import Effect.System


implementation Show Difficulty where
  show Easy = "Easy"
  show Medium = "Medium"
  show Hard = "Hard"


intro : String
intro =  """
___  ________ _   _  _____ _____  _    _ _____ ___________ ___________ 
|  \/  |_   _| \ | ||  ___/  ___|| |  | |  ___|  ___| ___ \  ___| ___ \
| .  . | | | |  \| || |__ \ `--. | |  | | |__ | |__ | |_/ / |__ | |_/ /
| |\/| | | | | . ` ||  __| `--. \| |/\| |  __||  __||  __/|  __||    /
| |  | |_| |_| |\  || |___/\__/ /\  /\  / |___| |___| |   | |___| |\ \
\_|  |_/\___/\_| \_/\____/\____/  \/  \/\____/\____/\_|   \____/\_| \_|
Written by Ross Meikleham 2015"""



invalid : String -> String
invalid s = "Unknown option \"" ++ s ++ "\". " ++ 
  "Enter h or help to display the list of possible options"

data GameAction = 
   GQuit
 | GHelp
 | GShow
 | Reveal Pos 
 | GInvalid String
 

parseGameAction : String -> GameAction
parseGameAction "q" =  GQuit
parseGameAction "quit" = GQuit
parseGameAction "h" = GHelp
parseGameAction "help" = GHelp
parseGameAction "d" = GShow
parseGameAction "display" = GShow
parseGameAction s = 
  case parseReveal (words s) of
    (Just pos) => Reveal pos
    (Nothing) => GInvalid (invalid s)

  where 
        parsePos : String -> String -> Maybe Pos
        parsePos strRow strCol = do
          row <- readNat strRow
          col <- readNat strCol
          pure $ MkPos col row
        
        parseReveal : List String -> Maybe Pos
        parseReveal (command :: row :: col :: []) =
          case command of
            "reveal" => parsePos row col
            "r" => parsePos row col

        parseReveal _ = Nothing



gHelp : Nat -> Nat -> String
gHelp (S r) (S c) = ("r [row] [column] or reveal [row] [column] to reveal the given square\n" ++
  "\t where row is between 0 and " ++ show r  ++ ", and column is between 0 and " ++ 
  show c ++ ".\n" ++
  "d or display to display the board\n" ++
  "q or quit to exit\n" ++
  "h or help to display help\n")
gHelp _ _ = "Error with row/column size\n"

playGame' : Board m n -> IO ()
playGame' {m} {n} board = do
          putStr "Enter Option> " 
          str <- getLine
          let option = parseGameAction str
          case option of
            (Reveal pos) => let (res, newBoard) = runState (revealPos pos) board in
                  case res of
                    Playing str => putStrLn (showBoard newBoard) >>= \_ => putStrLn str 
                                >>= \_ => playGame' newBoard

                    Won => putStrLn (showRevealed newBoard) >>= \_ => putStrLn  "You Win!"
                    Lost => putStrLn (showBoard newBoard) >>= \_ => putStrLn  "You Hit a Mine!" >>=  
                      \_ => putStrLn (showRevealed newBoard)

            GHelp => putStrLn (gHelp m n) >>= \_ => playGame' board 
            GQuit => putStrLn "Quitting Game..."
            GShow => putStrLn (showBoard board)
            GInvalid s => putStrLn s >>= \_ => playGame' board


playGame : Difficulty -> IO()
playGame difficulty = do

  let ((MkPos x y), nMines) = getSetupDetails difficulty
  mines <- run (generateMines x y nMines) 
  case mines of
    Nothing => putStrLn "More mines than positions available"
    Just m => do
      let board = createBoard x y m
      case board of
        Nothing => putStrLn "Error creating board"
        Just b => putStrLn (showBoard b) >>= \_ => playGame' b


-- | Difficulty Options/Menu
data DifficultyAction =
    DSelected Difficulty 
  | DHelp
  | DQuit
  | DInvalid String

difficulty : String
difficulty = "Enter difficulty> "


difficultyHelp : String
difficultyHelp = """
b or beginner to start an easy game
i or intermediate to start a medium difficulty game
e or expert to start a hard game
h or help to display this help option
q or quit to exit to the main menu"""

        
parseDifficulty : String -> DifficultyAction
parseDifficulty "b" = DSelected Easy
parseDifficulty "beginner" = DSelected Easy

parseDifficulty "i" = DSelected Medium
parseDifficulty "intermediate" = DSelected Medium

parseDifficulty "e" = DSelected Hard
parseDifficulty "expert" = DSelected Hard

parseDifficulty "h" = DHelp
parseDifficulty "help" = DHelp

parseDifficulty "q" = DQuit
parseDifficulty "quit" = DQuit

parseDifficulty s = DInvalid s


difficultyMenu : IO ()
difficultyMenu = do
  putStrLn ""
  putStr difficulty
  optionStr <- getLine
  let option = parseDifficulty optionStr
  case option of
    DSelected d => putStrLn ("Starting " ++ show d  ++ " game...") >>= \_ => playGame d
    DHelp => putStrLn difficultyHelp >>= \_ => difficultyMenu
    DQuit => putStrLn "Returning to main menu..."
    DInvalid s => (putStrLn $ invalid s) >>= \_ => difficultyMenu -- | Main Menu options


data MainMenuAction = 
    Quit
  | Play 
  | Help
  | Invalid String
  
parseOption : String -> MainMenuAction 
parseOption "q" = Quit
parseOption "quit" = Quit

parseOption "p" = Play
parseOption "play" = Play

parseOption "h" = Help
parseOption "help" = Help 

parseOption i = Invalid i

help : String
help = """
p or play to start a game
q or quit to exit
h or help to display help"""


mainMenu : IO ()
mainMenu = do
  putStrLn ""
  putStr "Enter option> "
  optionStr <- getLine
  let option = parseOption optionStr
  case option of
    Play => difficultyMenu >>= \_ => mainMenu
    Help => putStrLn help >>= \_ => mainMenu
    Quit => putStrLn "Goodbye :)"
    Invalid s => putStrLn (invalid s) >>= \_ => mainMenu



export
repl : IO ()
repl = do
  putStrLn intro
  mainMenu
