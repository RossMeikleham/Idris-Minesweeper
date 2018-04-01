# Idris-Minesweeper
[![build Status](https://travis-ci.org/RossMeikleham/Idris-Minesweeper.svg?branch=master)](https://travis-ci.org/RossMeikleham/Idris-Minesweeper)

A simple Minesweeper clone using a command line REPL for IO.
I created this as an exercise to learn the Idris programming language.

# Features
* 3 difficulty levels 
  * Beginner: 8x8 board with 10 mines
  * Intermediate: 16x16 board with 40 mines
  * Expert: 16x30 board with 99 mines

# TODO
- Possible GUI and/or ncurses IO implementation 

# Required
- Idris 0.9.19 (see the [Idris-Dev](https://github.com/idris-lang/Idris-dev/) repository) 

# Building The Package
`idris --build minesweeper.ipkg`

Enter "h" or "help" at anytime in the REPL to display a list of options and what they do.

# Running with Docker
```
docker build -t idris-minesweeper .
docker run --entrypoint="/app/minesweeper" idris-minesweeper
```

# Images
![main](/images/play.png?raw=true)
![lose](/images/lose.png?raw=true)
