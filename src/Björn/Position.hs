module Björn.Core.Position where

import Björn.Core.Pieces

-- A fully specified position, given by all pieces on the board and additional information like king moves and who is to move.
data Position = Position {
    pieces :: [Piece],
    kingMoves :: [(Color, KingMoves)],
    toMove :: Color
} deriving (Eq, Show)

-- King special-moves are per-player (not per-king) and are thus stored with the player.
data KingMoves = KingMoves {
    hasKnight :: Bool,
    hasBoomerang :: Bool
} deriving (Eq, Show)
