{-# LANGUAGE BangPatterns #-}

module Chessboard
 (
    PieceType(..),
    Piece(..),
    Chessboard
    ,
    nextMove,
    switch,
    emptyBoard,
    initialPosition,
    at,
    update,
    remove,
    toList,
    movePiece,
    color
) where

import qualified Data.Char as C
import qualified Data.Vector as V
import Color
import Position

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq)

instance Show PieceType where
    show Pawn   = "p"
    show Knight = "n"
    show Bishop = "b"
    show Rook   = "r"
    show Queen  = "q"
    show King   = "k"

data Piece = Piece !Color !PieceType
    deriving Eq

instance Show Piece where
    show (Piece White t) = map C.toUpper $ show t
    show (Piece Black t) = show t

color :: Piece -> Color
color (Piece c _) = c

data Chessboard
     = Chessboard

    { toVector :: !(V.Vector (Maybe Piece))
    , nextMove :: !Color
    }

switch :: Chessboard
 -> Chessboard

switch !cb = Chessboard
 { toVector = toVector cb, nextMove = other $ nextMove cb }

-- Board initialization
instance Show Chessboard
 where
    show cb = unlines (V.toList $ V.reverse $ V.imap showLine (slice8 (toVector cb)))
        ++ "  " ++ concat (replicate 8 "+---") ++ "+\n    "
        ++ concatMap ((:[]) . showFile) [0..7] ++ " " ++ show (nextMove cb)
        where
        showLine :: Int -> V.Vector (Maybe Piece) -> String
        showLine rank v =
            "  " ++ concat (replicate 8 "+---") ++ "+\n" ++ (
            showRank rank : " | " ++                                         
            concatMap (\sq -> showSquare sq ++ " | ") (V.toList v))
        showSquare :: Maybe Piece -> String
        showSquare Nothing = " "
        showSquare (Just p) = show p
        showRank :: Int -> Char
        showRank r = C.chr $ C.ord '1' + r
        showFile f = C.chr $ C.ord 'a' + f
        slice8 :: V.Vector a -> V.Vector (V.Vector a)
        slice8 v | V.null v = V.empty
                 | otherwise = h `V.cons` slice8 t
              where (h, t) = V.splitAt 8 v

emptyBoard :: Color -> Chessboard

emptyBoard !firstPlayer = Chessboard
 { toVector = V.replicate 64 Nothing, nextMove = firstPlayer }

initialPosition :: Chessboard

initialPosition = Chessboard
 { toVector = V.fromList $ concat [whiteRearRow, whiteFrontRow, emptyRows, blackFrontRow, blackRearRow], nextMove = White }
    where
    whiteRearRow  = map (Just . Piece White) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    whiteFrontRow = replicate 8 $ Just $ Piece White Pawn
    emptyRow      = replicate 8 Nothing
    emptyRows     = concat (replicate 4 emptyRow)
    blackFrontRow = replicate 8 $ Just $ Piece Black Pawn
    blackRearRow  = map (Just . Piece Black) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- Getting the piece to the given position
at :: Chessboard
 -> Position -> Maybe Piece
at !cb !p = if i >= 0 && i < 64 then toVector cb V.! i else Nothing
    where i = toIndex p

update :: Position -> Piece -> Chessboard
 -> Chessboard

update !pos !piece !cb = cb { toVector = toVector cb V.// [(i, Just piece)] }
    where i = toIndex pos

remove :: Position -> Chessboard
 -> Chessboard

remove !pos !cb = cb { toVector = toVector cb V.// [(i, Nothing)] }
    where i = toIndex pos

toList :: Chessboard
 -> [Maybe Piece]
toList !cb = V.toList $ toVector cb

movePiece :: Chessboard
 -> Position -> Position -> Chessboard

movePiece board from to =
    case at board from of
        Nothing -> board
        Just piece ->
            case at board to of
                Just targetPiece -> if color piece /= color targetPiece
                                    then update to piece $ remove from board
                                    else board
                Nothing -> update to piece $ remove from board
