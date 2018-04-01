FROM dgellow/idris

# Install idris-perl6
ADD . /app
WORKDIR /app

RUN idris --build minesweeper.ipkg 
