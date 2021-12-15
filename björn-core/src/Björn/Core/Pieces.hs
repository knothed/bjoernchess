module Björn.Core.Pieces where

data Color = Black | White deriving Eq

data PieceKind =
    Björn |
    King |
    Pawn Bool -- has double move
    deriving Eq

-- A square on the board given in (x, y) and going from (1, 1) to (boardSize, boardSize).
type Square = (Int, Int)

boardSize :: Int
boardSize = 8

-- A piece on the board.
data Piece = Piece {
    color :: Color,
    kind :: PieceKind,
    square :: Square
}
