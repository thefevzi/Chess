# Chess
Simple Chess Game in Haskell with excluding the rules en passant and threefold repetition for a draw.
This is a work in progress for now, it features only human vs human mod.

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
$ cabal run chess 1 or cabal run chess 2
```
cabal run chess 1, starts Human vs Human mod and cabal run chess 2, starts Human vs AI


## Features to be Added
• AI which uses minimax algorithm, enhanced by alpha-beta pruning
• The depth of search for AI
