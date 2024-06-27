{-# LANGUAGE BangPatterns #-}

module Moves (
    isValidMove,
    isValidCastleMove,
    isInCheck
) where

import Chessboard
import Position
import Color
import Data.Maybe (isJust, isNothing, fromJust)

-- Helper functions for straight and diagonal moves
isValidStraightMove :: Chessboard -> Position -> Position -> Bool
isValidStraightMove board (Position r1 f1) (Position r2 f2)
    | r1 == r2 = all (isNothing . at board) [(Position r1 f)
     | f <- [min f1 f2 + 1 .. max f1 f2 - 1]]
    | f1 == f2 = all (isNothing . at board) [(Position r f1)
     | r <- [min r1 r2 + 1 .. max r1 r2 - 1]]
    | otherwise = False

isValidDiagonalMove :: Chessboard -> Position -> Position -> Bool
isValidDiagonalMove board (Position r1 f1) (Position r2 f2)
    | abs (r2 - r1) == abs (f2 - f1) =
    all (isNothing . at board)
     [(Position (r1 + i * dr) (f1 + i * df)) | i <- [1 .. abs (r2 - r1) - 1]]
    | otherwise = False
    where
        dr = if r2 > r1 then 1 else -1
        df = if f2 > f1 then 1 else -1

-- Checking ValidPawnMove for White and Black pawns
isValidPawnMove' :: Chessboard -> Color -> Position -> Position -> Bool
isValidPawnMove' board color (Position r1 f1) (Position r2 f2)
    | r2 == r1 + direction && f1 == f2 && isNothing (at board (Position r2 f2)) = True 
    | r1 == initialRank && r2 == r1 + 2 * direction && f1 == f2 &&
     isNothing (at board (Position (r1 + direction) f1))
      && isNothing (at board (Position r2 f2)) = True 
    | r2 == r1 + direction && abs (f2 - f1) == 1 && isJust (at board (Position r2 f2)) = True
    | otherwise = False
  where
    (direction, initialRank) = if color == White then (1, 1) else (-1, 6)

isValidPawnMove :: Chessboard -> Position -> Position -> Bool
isValidPawnMove board from to =
    case at board from of
        Just (Piece color Pawn) -> isValidPawnMove' board color from to
        _ -> False

isValidKnightMove :: Chessboard -> Position -> Position -> Bool
isValidKnightMove _ (Position r1 f1) (Position r2 f2) =
    let dr = abs (r2 - r1)
        df = abs (f2 - f1)
    in (dr == 2 && df == 1) || (dr == 1 && df == 2)

isValidBishopMove :: Chessboard -> Position -> Position -> Bool
isValidBishopMove = isValidDiagonalMove

isValidRookMove :: Chessboard -> Position -> Position -> Bool
isValidRookMove = isValidStraightMove

isValidQueenMove :: Chessboard -> Position -> Position -> Bool
isValidQueenMove board from to =
     isValidStraightMove board from to || isValidDiagonalMove board from to

isValidKingMove :: Chessboard -> Position -> Position -> Bool
isValidKingMove _ (Position r1 f1) (Position r2 f2) =
    let dr = abs (r2 - r1)
        df = abs (f2 - f1)
    in dr <= 1 && df <= 1

isValidPieceMove :: Chessboard -> Position -> Position -> Bool
isValidPieceMove board from to =
    case at board from of
        Just (Piece _ Pawn)   -> isValidPawnMove board from to
        Just (Piece _ Knight) -> isValidKnightMove board from to
        Just (Piece _ Bishop) -> isValidBishopMove board from to
        Just (Piece _ Rook)   -> isValidRookMove board from to
        Just (Piece _ Queen)  -> isValidQueenMove board from to
        Just (Piece _ King)   -> isValidKingMove board from to
        _ -> False

-- Checks if a piece can attack a position
canAttack :: Chessboard -> Position -> Position -> Bool
canAttack board target from =
    case at board from of
        Nothing -> False
        Just piece -> isValidPieceMove (update target (fromJust (at board from)) board) from target

-- Get all the piece positions for opponent
opponentPieces :: Chessboard -> Color -> [Position]
opponentPieces board color = [pos | (pos, Just (Piece c _))
 <- zip [Position r f | r <- [0..7], f <- [0..7]] (toList board), c == color]

-- Find the position of the king of the given color
kingPos :: Chessboard -> Color -> Maybe Position
kingPos board color =
    case [pos | (pos, Just (Piece c King))
     <- zip [Position r f | r <- [0..7], f <- [0..7]] 
     (toList board), c == color] of
        []     -> Nothing
        (k:_)  -> Just k

isInCheck :: Chessboard -> Color -> Bool
isInCheck board color =
    case kingPos board color of
        Nothing -> False
        Just kp -> any (canAttack board kp) (opponentPieces board (other color))

leavesKingInCheck :: Chessboard -> Position -> Position -> Bool
leavesKingInCheck board from to =
    let newBoard = movePiece board from to
    in isInCheck newBoard (nextMove board)

-- Validate Move
isValidMove :: Chessboard -> Position -> Position -> Bool
isValidMove board from to =
    isJust (at board from) &&
    from /= to &&
    isValidPieceMove board from to &&
    not (leavesKingInCheck board from to) &&
    (all (\p -> color p /= color (fromJust (at board from))) (at board to))

-- Castling
isValidCastleMove :: Chessboard -> Position -> Position -> Bool
isValidCastleMove board (Position r1 f1) (Position r2 f2)
    | r1 /= r2 = False
    | abs (f2 - f1) /= 2 = False
    | otherwise =
        let nextMoveColor = nextMove board
            rookPos = if f2 > f1 then Position r1 7 else Position r1 0
            rook = at board rookPos
            betweenPositions = if f2 > f1 then [Position r1 (f1 + 1), Position r1 (f1 + 2)] else [Position r1 (f1 - 1), Position r1 (f1 - 2), Position r1 (f1 - 3)]
            emptyBetween = all (isNothing . at board) betweenPositions
            kingNotMoved = nextMoveColor `notElem` kingsMoved board
            rookNotMoved = (nextMoveColor, file rookPos) `notElem` rooksMoved board
            noCheck = all (\pos -> not (isInCheck (movePiece board (Position r1 f1) pos) (nextMove board))) (Position r1 f1 : betweenPositions)
            -- Only king can castle
            isKing = case at board (Position r1 f1) of
                        Just (Piece _ King) -> True
                        _ -> False
        in isKing && isJust rook && color (fromJust rook) == nextMoveColor && emptyBetween && kingNotMoved && rookNotMoved && noCheck
