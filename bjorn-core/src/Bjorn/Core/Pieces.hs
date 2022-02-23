module Bjorn.Core.Pieces where

-- A square on the board given in (x, y) and going from (1, 1) to (boardSize, boardSize).
type Square = (Int, Int)

boardSize :: Int
boardSize = 8

validSquare :: Square -> Bool
validSquare (x, y) = between 1 boardSize x && between 1 boardSize y where
    between a b x = a <= x && x <= b

-- The metric induced by the sup-norm on R^2.
dist :: Square -> Square -> Int
dist a b = max (abs (fst a - fst b)) (abs (snd a - snd b))

-- Color and Pieces

data Color = Black | White deriving (Eq, Show)

opp White = Black
opp Black = White

-- The y-direction in which pawns move.
mvmtDir White = 1
mvmtDir Black = -1

data Piece =
    Bjorn |
    King |
    Pawn Bool -- has double move
    deriving (Eq, Show)

isPawn (Pawn _) = True
isPawn _ = False