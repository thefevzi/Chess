module Color (
    Color(..),
    other
) where

data Color = White | Black
    deriving (Eq, Show)
-- Convert Color to string TODO, Done by Show

other :: Color -> Color
other White = Black
other Black = White

-- Eq comprassion by using == and /=
--
 