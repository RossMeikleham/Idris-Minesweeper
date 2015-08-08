# Idris-Minesweeper
A simple Minesweeper clone using a command line REPL for IO.
I created this as an exercise to learn the Idris programming language.

# Features
* 3 difficulty levels 
  * Beginner: 8x8 board with 10 mines
  * Intermediate: 16x16 board with 40 mines
  * Expert: 16x30 board with 99 mines

#TODO
- Cascading reveal algorithm implementation (i.e. when revealing a square which surrounds no mines then all
the squares surrounding that square are then revealed)
- Setting RNG seed for generating mine positions
- Possible GUI and/or ncurses IO implementation 

#Required
- Idris (see the [Idris-Dev](https://github.com/idris-lang/Idris-dev/) repository) 

# Building The Package
`idris --build minesweeper.ipkg`

Enter "h" or "help" at anytime in the REPL to display a list of options and what they do.

#Images
![main](/images/play.png?raw=true)
![lose](/images/lose.png?raw=true)
