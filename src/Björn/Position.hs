module Björn.Position where

import Utils.Maybe2

-- A square on the board given in (x, y) and going from (1, 1) to (8, 8).
type Square = (Int, Int)

-- The position of a single player on the board.
data PlayerPosition = PlayerPosition
                    { pawns :: Maybe2 PawnPos
                    , king :: Maybe KingPos
                    , hasBoomerangMove :: Bool
                    , hasKnightMove :: Bool
                    , björn :: BjörnPos
                    } deriving (Show, Eq)

data PawnPos = PawnPos { pos :: Square, hasDoubleMove :: Bool } deriving (Show, Eq)
type KingPos = Square
type BjörnPos = Square

-- A fully specified position.
data Position = Position { white :: PlayerPosition, black :: PlayerPosition }