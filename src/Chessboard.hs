{-# LANGUAGE BangPatterns #-}

module Chessboard
 (
    PieceType(..),
    Piece(..),
    Chessboard,
    nextMove,
    switch,
    initialPosition,
    at,
    update,
    toList,
    movePiece,
    movePieceCastling,
    color,
    didKingMove,
    didRookMove
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

data Piece = Piece Color PieceType
    deriving Eq

instance Show Piece where
    show (Piece White t) = map C.toUpper $ show t
    show (Piece Black t) = show t

color :: Piece -> Color
color (Piece c _) = c

data Chessboard = Chessboard
    { toVector :: V.Vector (Maybe Piece)
    , nextMove :: Color
    , didKingMove :: (Bool, Bool)
    , didRookMove :: ((Bool, Bool), (Bool, Bool)) --W,L,F . B,L,R 
    }


switch :: Chessboard -> Chessboard
switch cb = cb
    { nextMove = other (nextMove cb) }

-- Board initialization
instance Show Chessboard where
    show cb = unlines (V.toList $ V.reverse $ V.imap showLine (slice8 (toVector cb)))
        ++ "  " ++ concat (replicate 8 "+---") ++ "+\n    "
        ++ concatMap ((\f -> f ++ "   ") . (:[]) . showFile) [0..7] ++
        " " ++ "\n" ++ show (nextMove cb)
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
emptyBoard firstPlayer = Chessboard
    { toVector = V.replicate 64 Nothing
    , nextMove = firstPlayer
    , didKingMove = (False, False)
    , didRookMove = ((False, False), (False, False))
    }

initialPosition :: Chessboard
initialPosition = Chessboard
    { toVector = V.fromList $ concat [whiteRearRow, whiteFrontRow, emptyRows, blackFrontRow, blackRearRow]
    , nextMove = White
    , didKingMove = (False, False)
    , didRookMove = ((False, False), (False, False))
    }
    where
    whiteRearRow  = map (Just . Piece White) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    whiteFrontRow = replicate 8 $ Just $ Piece White Pawn
    emptyRow      = replicate 8 Nothing
    emptyRows     = concat (replicate 4 emptyRow)
    blackFrontRow = replicate 8 $ Just $ Piece Black Pawn
    blackRearRow  = map (Just . Piece Black) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]


-- Getting the piece to the given position
at :: Chessboard -> Position -> Maybe Piece
at cb p = if i >= 0 && i < 64 then toVector cb V.! i else Nothing
    where i = toIndex p

update :: Position -> Piece -> Chessboard -> Chessboard
update pos piece cb = cb { toVector = toVector cb V.// [(i, Just piece)] }
    where i = toIndex pos

remove :: Position -> Chessboard -> Chessboard
remove pos cb = cb { toVector = toVector cb V.// [(i, Nothing)] }
    where i = toIndex pos

toList :: Chessboard -> [Maybe Piece]
toList cb = V.toList $ toVector cb

-- Pawn Promotion --
promotePawn :: Piece -> PieceType -> Piece
promotePawn (Piece color Pawn) newType = Piece color newType
promotePawn piece _ = piece 

-- Castling setting kings and rooks positions, checking for movemenent
kingSet :: Color -> Chessboard -> Chessboard
kingSet White cb = cb { didKingMove = (True, snd (didKingMove cb)) }
kingSet Black cb = cb { didKingMove = (fst (didKingMove cb), True) }

rookSet :: Color -> Int -> Chessboard -> Chessboard
rookSet White index cb
    | index == 0 = cb { didRookMove = ((True, snd (fst (didRookMove cb))), snd (didRookMove cb)) }
    | index == 7 = cb { didRookMove = ((fst (fst (didRookMove cb)), True), snd (didRookMove cb)) }
rookSet Black index cb
    | index == 56 = cb { didRookMove = (fst (didRookMove cb), (True, snd (snd (didRookMove cb)))) }
    | index == 63 = cb { didRookMove = (fst (didRookMove cb), (fst (snd (didRookMove cb)), True)) }

updateFlags :: Piece -> Position -> Chessboard -> Chessboard
updateFlags (Piece color King) from board = kingSet color board
updateFlags (Piece color Rook) from board
    | from == Position 0 0 = rookSet color 0 board
    | from == Position 0 7 = rookSet color 7 board
    | from == Position 7 0 = rookSet color 56 board
    | from == Position 7 7 = rookSet color 63 board
    | otherwise = board
updateFlags _ _ board = board


movePiece :: Chessboard -> Position -> Position -> Chessboard
movePiece board from to =
    case at board from of
        Nothing -> board
        Just piece ->
            if color piece /= nextMove board
            then board
            else
                let board' = update to piece $ remove from board
                    board'' = if promotion piece to
                              then update to (promotePawn piece Queen) board'
                              else board'
                in updateFlags piece from board''
  where
    promotion (Piece White Pawn) (Position 0 _) = True
    promotion (Piece Black Pawn) (Position 7 _) = True
    promotion _ _ = False

movePieceCastling :: Chessboard -> Position -> Position -> Chessboard
movePieceCastling board from to =
    case (at board from, at board rookFrom) of
        (Just king, Just rook) ->
            let board1 = update to king (remove from board)
                board2 = update rookTo rook (remove rookFrom board1)
            in kingSet (color king) (rookSet (color rook) (toIndex rookFrom) board2)
        _ -> board
    where
        rookFrom = if file to > file from then Position (rank from) 7 else Position (rank from) 0
        rookTo = Position (rank from) ((file from + file to + 1) `div` 2)
