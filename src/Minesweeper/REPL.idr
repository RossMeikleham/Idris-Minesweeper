
-- | REPL for console version of Minesweeper
module Minesweeper.REPL

import Minesweeper.Game
import Minesweeper.Board
import Minesweeper.Helper

import Control.Monad.State

import Effects
import Effect.Random
import Effect.StdIO


instance Show Difficulty where
  show Easy = "Easy"
  show Medium = "Medium"
  show Hard = "Hard"


intro : String
intro =  "___  ________ _   _  _____ _____  _    _ _____ ___________ ___________\n\ 
         \|  \\/  |_   _| \\ | ||  ___/  ___|| |  | |  ___|  ___| ___ \\  ___| ___ \\\n\
         \| .  . | | | |  \\| || |__ \\ `--. | |  | | |__ | |__ | |_/ / |__ | |_/ /\n\ 
         \| |\\/| | | | | . ` ||  __| `--. \\| |/\\| |  __||  __||  __/|  __||    /\n\ \| |  | |_| |_| |\\  || |___/\\__/ /\\  /\\  / |___| |___| |   | |___| |\\ \\\n\
         \\\_|  |_/\\___/\\_| \\_/\\____/\\____/  \\/  \\/\\____/\\____/\\_|   \\____/\\_| \\_|\n\
         \\n Written by Ross Meikleham 2015"



invalid : String -> String
invalid s = "Unknown option \"" ++ s ++ "\"\n\
                      \Enter h or help to display the list of possible options"

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
    (Just Pos) => Reveal Pos
    (Nothing) => GInvalid (invalid s)

  where 
        parsePos : String -> String -> Maybe Pos
        parsePos strRow strCol = do
          row <- readNat strRow
          col <- readNat strCol
          return $ MkPos col row
        
        parseReveal : List String -> Maybe Pos
        parseReveal (command :: row :: col :: []) =
          case command of
            "reveal" => parsePos row col
            "r" => parsePos row col

        parseReveal _ = Nothing



gHelp : Nat -> Nat -> String
gHelp r c= "r [row] [column] or reveal [row] [column] to reveal the given square\n\
        \\t where row is between 0 and " ++ show (r - 1) ++ ", and column is between\ 
        \ 0 and " ++ show (c - 1) ++ ".\n\
        \d or display to display the board\n\
        \q or quit to exit\n\
        \h or help to display help\n"


playGame' : Board m n -> IO ()
playGame' {m} {n} board = do
          putStr "Enter Option> " 
          str <- getLine
          let option = parseGameAction str
          case option of
            (Reveal pos) => let (res, newBoard) = runState (reveal pos) board in
                  case res of
                    Playing str => putStrLn (showBoard newBoard) >>= \_ => putStrLn str 
                                >>= \_ => playGame' newBoard

                    Won => putStrLn (showRevealed newBoard) >>= \_ => putStrLn  "You Win!"
                    Lost => putStrLn (showRevealed newBoard) >>= \_ => putStrLn  "You Hit a Mine!"

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
difficultyHelp = "b or beginner to start an easy game\n\
                 \i or intermediate to start a medium difficulty game\n\
                 \e or expert to start a hard game\n\
                 \h or help to display this help option\n\
                 \q or quit to exit to the main menu\n"

        
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
help = "p or play to start a game\n\
       \q or quit to exit\n\
       \h or help to display help\n"


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




repl : IO ()
repl = do
  putStrLn intro
  mainMenu
