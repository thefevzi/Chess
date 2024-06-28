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
    color,
    kingsMoved,
    rooksMoved,
) where

import qualified Data.Char as C
import qualified Data.Vector as V

import Color
import Position

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord)

instance Show PieceType where
    show Pawn   = "p"
    show Knight = "n"
    show Bishop = "b"
    show Rook   = "r"
    show Queen  = "q"
    show King   = "k"

data Piece = Piece Color PieceType
    deriving (Eq, Ord)

instance Show Piece where
    show (Piece White t) = map C.toUpper $ show t
    show (Piece Black t) = show t

color :: Piece -> Color
color (Piece c _) = c

data Chessboard = Chessboard
    { toVector :: V.Vector (Maybe Piece)
    , nextMove :: Color
    , kingsMoved :: [Color]
    , rooksMoved :: [(Color, Int)]
    } deriving (Eq, Ord)

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
    , kingsMoved = []
    , rooksMoved = []
    }

initialPosition :: Chessboard
initialPosition = Chessboard
    { toVector = V.fromList $ concat [whiteRearRow, whiteFrontRow, emptyRows, blackFrontRow, blackRearRow]
    , nextMove = White
    , kingsMoved = []
    , rooksMoved = []
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

updateFlags :: Piece -> Position -> Chessboard -> Chessboard
updateFlags (Piece color King) _ board = board { kingsMoved = color : kingsMoved board }
updateFlags (Piece color Rook) from board = board { rooksMoved = (color, file from) : rooksMoved board }
updateFlags _ _ board = board

movePieceCastling :: Chessboard -> Position -> Position -> Chessboard
movePieceCastling board from to =
    case (at board from, at board rookFrom) of
        (Just king, Just rook) ->
            let board1 = update to king (remove from board)
                board2 = update rookTo rook (remove rookFrom board1)
            in updateFlags king from $ updateFlags rook rookFrom board2
        _ -> board
    where
        rookFrom = if file to > file from then Position (rank from) 7 else Position (rank from) 0
        rookTo = Position (rank from) ((file from + file to + 1) `div` 2)

movePiece :: Chessboard -> Position -> Position -> Chessboard
movePiece board from to =
    case at board from of
        Nothing -> board
        Just piece ->
            if color piece /= nextMove board
            then board
            else if isCastlingMove piece from to
                 then movePieceCastling board from to
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

    isCastlingMove (Piece _ King) (Position r1 f1) (Position r2 f2) =
        r1 == r2 && abs (f2 - f1) == 2
    isCastlingMove _ _ _ = False
