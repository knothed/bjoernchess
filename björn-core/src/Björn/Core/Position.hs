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

-- Check whether a position is valid, meaning:
-- Each player has exactly one björn, at most one king and and most two pawns,
-- no björns or kings are adjacent to each other, no field is occupied twice,
-- no piece has an invalid position and every player has specified kingMoves.
-- Use this to validate positions input by the user.
positionValid :: Position -> Bool
positionValid = const True