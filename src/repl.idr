
module Main --Minesweeper.REPL


data Difficulty = Easy | Medium | Hard

instance Show Difficulty where
  show Easy = "Easy"
  show Medium = "Medium"
  show Hard = "Hard"

data Action = 
    Quit
  | Play 
  | Help
  | Invalid


intro : String
intro =  "___  ________ _   _  _____ _____  _    _ _____ ___________ ___________\n\ 
         \|  \\/  |_   _| \\ | ||  ___/  ___|| |  | |  ___|  ___| ___ \\  ___| ___ \\\n\
         \| .  . | | | |  \\| || |__ \\ `--. | |  | | |__ | |__ | |_/ / |__ | |_/ /\n\ 
         \| |\\/| | | | | . ` ||  __| `--. \\| |/\\| |  __||  __||  __/|  __||    /\n\
         \| |  | |_| |_| |\\  || |___/\\__/ /\\  /\\  / |___| |___| |   | |___| |\\ \\\n\
         \\\_|  |_/\\___/\\_| \\_/\\____/\\____/  \\/  \\/\\____/\\____/\\_|   \\____/\\_| \\_|\n\
         \\n Written by Ross Meikleham 2015"

--getOption : IO (String)
--getOption = do
--  putStr ">"
--  getStrLn 
  
parseOption : String -> Action 
parseOption "q" = Quit
parseOption "quit" = Quit

parseOption "p" = Play
parseOption "play" = Play

parseOption "h" = Help
parseOption "help" = Help 


help : String
help = "p or play to start a game\n\
       \q or quit to exit\n\
       \h or help to display help\n"


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

difficultyInvalid : String -> String
difficultyInvalid s = "Unknown option \"" ++ s ++ "\"\n\
                      \Enter h or help to display the list of possible options"
        
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
    DSelected d => putStrLn ("Starting " ++ show d  ++ " game...") >>= \_ => difficultyMenu
    DHelp => putStrLn difficultyHelp >>= \_ => difficultyMenu
    DQuit => putStrLn "Returning to main menu..."
    DInvalid s => (putStrLn $ difficultyInvalid s) >>= \_ => difficultyMenu


main : IO ()
main = difficultyMenu --putStrLn intro
