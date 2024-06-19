{-# LANGUAGE BangPatterns #-}

module WinCheck (
    isCheckmate
) where

import Chessboard
import Color
import Position
import Moves (isValidMove, isInCheck)

-- Check if there is a checkmate?
isCheckmate :: Chessboard -> Color -> Bool
isCheckmate board color = isInCheck board color && noLegalMoves board color

-- Checking if there are any legal moves?
noLegalMoves :: Chessboard -> Color -> Bool
noLegalMoves board color = all (== False) [isValidMove board from to | from <- ownPieces, to <- allPositions]
    where
        ownPieces = [pos | (pos, Just (Piece c _)) <- zip [Position r f | r <- [0..7], f <- [0..7]] (toList board), c == color]
        allPositions = [Position r f | r <- [0..7], f <- [0..7]]
