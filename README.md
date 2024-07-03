# Chess
Simple Chess Game in Haskell with excluding the rules en passant and threefold repetition for a draw.
This is a work in progress for now, AI is not making very healthy decisions

# Description
I have created this project for my Non-procedural Programming class as a semester project. The game can be played in two mods:
1. Human vs Human
2. Human vs AI

Project mainly focuses on core gameplay mechanics with using command line interface

# How to Run
`cabal` is required to play the game

```sh
$ git clone https://github.com/0Megalodon/Chess.git
$ cabal build
$ cabal run chess 1 or cabal run chess 2 <depth>
```
cabal run chess 1 , starts Human vs Human mod <br />
cabal run chess 2 "depth" , starts Human vs AI mod

## Features to be Added
• AI which uses minimax algorithm, enhanced by alpha-beta pruning [X] <br />
• The depth of search for AI [X] <br />
• cabal arguments [X] <br />
• Castling [X] <br />
• Pawn promotion [X] <br />
• Pinned check [X] <br />
• Stalemate [X] <br />
• Pawn to not hope over pieces [X]
