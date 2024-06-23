module AI where

import Chessboard
import Position
import Color
import CheckEnd
import Moves

valueOfaPiece :: Piece -> Int
valueOfaPiece (Piece _ Pawn)   = 1
valueOfaPiece (Piece _ Knight) = 3
valueOfaPiece (Piece _ Bishop) = 3
valueOfaPiece (Piece _ Rook)   = 5
valueOfaPiece (Piece _ Queen)  = 9
valueOfaPiece (Piece _ King)   = 9999

-- TODO : Minimax (recursive approach, alpha-beta), Neutral Evaluation (b,w), 