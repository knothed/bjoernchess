module Björn.Core.Pieces where

boardSize :: Int
boardSize = 8

-- Square

-- A square on the board given in (x, y) and going from (1, 1) to (boardSize, boardSize).
type Square = (Int, Int)

squareValid :: Square -> Bool
squareValid (x, y) = between 1 boardSize x && between 1 boardSize y where
    between a b x = a <= x && x <= b

-- The metric induced by the sup-norm on R^2.
distance :: Square -> Square -> Int
distance a b = max (abs (fst a - fst b)) (abs (snd a - snd b))

-- Color and Pieces

data Color = Black | White deriving (Eq, Show)

data PieceKind =
    Björn |
    King |
    Pawn Bool -- has double move
    deriving (Eq, Show)

-- A piece on the board.
data Piece = Piece {
    color :: Color,
    kind :: PieceKind,
    square :: Square
} deriving (Eq, Show)