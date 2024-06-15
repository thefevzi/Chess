{-# LANGUAGE BangPatterns #-}

module Moves (
    isValidPieceMove,
    isValidMove,
    leavesKingInCheck,
    isValidKnightMove,
    isValidPawnMove,
    isValidWhitePawnMove,
    isValidBlackPawnMove,
    isValidBishopMove,
    isValidRookMove,
    isValidQueenMove,
    isValidKingMove,
    isValidStraightMove,
    isValidDiagonalMove,
    isInCheck
) where

import ChessBoard
import Position
import Color
import Data.Maybe (isJust, isNothing)

-- Piece movement validation
isValidPieceMove :: ChessBoard -> Position -> Position -> Bool
isValidPieceMove board from to =
    case at board from of
        Just (Piece _ Pawn)   -> isValidPawnMove board from to
        Just (Piece _ Knight) -> isValidKnightMove board from to
        Just (Piece _ Bishop) -> isValidBishopMove board from to
        Just (Piece _ Rook)   -> isValidRookMove board from to
        Just (Piece _ Queen)  -> isValidQueenMove board from to
        Just (Piece _ King)   -> isValidKingMove board from to
        _ -> False

-- Function to simulate a move and check if it leaves the king in check
leavesKingInCheck :: ChessBoard -> Position -> Position -> Bool
leavesKingInCheck board from to =
    let newBoard = movePiece board from to
    in isInCheck newBoard (nextMove board)

-- Function to validate a move considering the rules and check condition
isValidMove :: ChessBoard -> Position -> Position -> Bool
isValidMove board from to =
    isJust (at board from) &&
    (toIndex from /= toIndex to) &&
    isValidPieceMove board from to &&
    not (leavesKingInCheck board from to)

-- Check if a given color is in check
isInCheck :: ChessBoard -> Color -> Bool
isInCheck board color = any (isThreatened board (kingPos board color)) (opponentPieces board (other color))

-- Piece-specific move validations
isValidKnightMove :: ChessBoard -> Position -> Position -> Bool
isValidKnightMove _ (Position r1 f1) (Position r2 f2) =
    let dr = abs (r2 - r1)
        df = abs (f2 - f1)
    in (dr == 2 && df == 1) || (dr == 1 && df == 2)

isValidPawnMove :: ChessBoard -> Position -> Position -> Bool
isValidPawnMove board from to =
    case at board from of
        Just (Piece White Pawn) -> isValidWhitePawnMove board from to
        Just (Piece Black Pawn) -> isValidBlackPawnMove board from to
        _ -> False

isValidWhitePawnMove :: ChessBoard -> Position -> Position -> Bool
isValidWhitePawnMove board (Position r1 f1) (Position r2 f2)
    | r2 == r1 + 1 && f1 == f2 && isNothing (at board (Position r2 f2)) = True -- Move forward
    | r1 == 1 && r2 == 3 && f1 == f2 && isNothing (at board (Position r2 f2)) = True -- Initial double move
    | r2 == r1 + 1 && abs (f2 - f1) == 1 && isJust (at board (Position r2 f2)) = True -- Capture
    | otherwise = False

isValidBlackPawnMove :: ChessBoard -> Position -> Position -> Bool
isValidBlackPawnMove board (Position r1 f1) (Position r2 f2)
    | r2 == r1 - 1 && f1 == f2 && isNothing (at board (Position r2 f2)) = True -- Move forward
    | r1 == 6 && r2 == 4 && f1 == f2 && isNothing (at board (Position r2 f2)) = True -- Initial double move
    | r2 == r1 - 1 && abs (f2 - f1) == 1 && isJust (at board (Position r2 f2)) = True -- Capture
    | otherwise = False

isValidBishopMove :: ChessBoard -> Position -> Position -> Bool
isValidBishopMove board from to = isValidDiagonalMove board from to

isValidRookMove :: ChessBoard -> Position -> Position -> Bool
isValidRookMove board from to = isValidStraightMove board from to

isValidQueenMove :: ChessBoard -> Position -> Position -> Bool
isValidQueenMove board from to =
    isValidStraightMove board from to || isValidDiagonalMove board from to

isValidKingMove :: ChessBoard -> Position -> Position -> Bool
isValidKingMove _ (Position r1 f1) (Position r2 f2) =
    let dr = abs (r2 - r1)
        df = abs (f2 - f1)
    in dr <= 1 && df <= 1

-- Helper functions for straight and diagonal moves
isValidStraightMove :: ChessBoard -> Position -> Position -> Bool
isValidStraightMove board (Position r1 f1) (Position r2 f2)
    | r1 == r2 = all (isNothing . at board) [(Position r1 f) | f <- [min f1 f2 + 1 .. max f1 f2 - 1]]
    | f1 == f2 = all (isNothing . at board) [(Position r f1) | r <- [min r1 r2 + 1 .. max r1 r2 - 1]]
    | otherwise = False

isValidDiagonalMove :: ChessBoard -> Position -> Position -> Bool
isValidDiagonalMove board (Position r1 f1) (Position r2 f2)
    | abs (r2 - r1) == abs (f2 - f1) = all (isNothing . at board) [(Position (r1 + i * dr) (f1 + i * df)) | i <- [1 .. abs (r2 - r1) - 1]]
    | otherwise = False
    where
        dr = if r2 > r1 then 1 else -1
        df = if f2 > f1 then 1 else -1

-- Find the position of the king of the given color
kingPos :: ChessBoard -> Color -> Position
kingPos board color = head [pos | (pos, Just (Piece c King)) <- zip [Position r f | r <- [0..7], f <- [0..7]] (toList board), c == color]

-- Get all opponent piece positions
opponentPieces :: ChessBoard -> Color -> [Position]
opponentPieces board color = [pos | (pos, Just (Piece c _)) <- zip [Position r f | r <- [0..7], f <- [0..7]] (toList board), c == color]

-- Check if a position is threatened by an opponent piece
isThreatened :: ChessBoard -> Position -> Position -> Bool
isThreatened board pos oppPos = isValidMove (switch board) oppPos pos
