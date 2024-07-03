module AI 
(
    bestMove
) where

import Chessboard
import Position
import Color
import CheckEnd
import Moves

-- Generating all legal moves for given color (noLegalMoves from CheckEnd.hs)--
generateMoves :: Chessboard -> Color -> [(Position, Position)]
generateMoves board color =
     [(from, to) | from <- ownPieces, to <- allPositions, isValidMove board from to]
  where
    ownPieces = [pos | (pos, Just (Piece c _))
     <- zip [Position r f | r <- [0..7], f <- [0..7]] (toList board), c == color]
    allPositions = [Position r f | r <- [0..7], f <- [0..7]]

evaluateBoard :: Chessboard -> Int
evaluateBoard board = sum [pieceValue piece | Just piece <- toList board]
  where
    pieceValue (Piece color Pawn)   = if color == White then -1 else 1
    pieceValue (Piece color Knight) = if color == White then -3 else 3
    pieceValue (Piece color Bishop) = if color == White then -3 else 3
    pieceValue (Piece color Rook)   = if color == White then -5 else 5
    pieceValue (Piece color Queen)  = if color == White then -9 else 9
    pieceValue (Piece color King)   = if color == White then -9999 else 9999

minimax :: Chessboard -> Color -> Int -> Int -> Int -> Int
minimax board color depth alpha beta

  | depth == 0 || isCheckmate board color || isStalemate board color = evaluateBoard board
  | color == Black = maximumValue moves alpha beta
  | color == White = minimumValue moves alpha beta

  where
    moves = generateMoves board color
    
    maximumValue [] a _ = a
    maximumValue ((from, to):rest) a b =
      let newBoard = switch $ movePiece board from to
          newAlpha = max a (minimax newBoard White (depth - 1) a b)
      in if newAlpha >= b then newAlpha else maximumValue rest newAlpha b

    minimumValue [] _ b = b
    minimumValue ((from, to):rest) a b =
      let newBoard = switch $ movePiece board from to
          newBeta = min b (minimax newBoard Black (depth - 1) a b)
      in if newBeta <= a then newBeta else minimumValue rest a newBeta

-- Selecting the best move for AI --
bestMove :: Chessboard -> Color -> Int -> (Position, Position)
bestMove board aiColor depth =
  let scoredMoves = [(minimax (switch $ movePiece board from to)
                    (other aiColor) (depth - 1) (-9999) 9999, (from, to))
                     | (from, to) <- moves]
  in snd $ maximum scoredMoves
  where
    moves = generateMoves board aiColor
