module Björn.Core.Position(
    Position(..), KingMoves(..),
    whiteBjörn, blackBjörn, whiteKing, blackKing, whitePawns, blackPawns,
    positionValid
) where

import Björn.Core.Pieces
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.List (find, group, sort)

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

-- Piece extractors. These assume that the position is valid.
pieceMatch knd col pc = kind pc == knd && color pc == col

theBjörn :: Color -> Position -> Piece
theBjörn col = fromJust . find (pieceMatch Björn col) . pieces
theKing col = find (pieceMatch King col) . pieces
thePawns col = filter (liftM2 (||) (pieceMatch (Pawn True) col) (pieceMatch (Pawn False) col)) . pieces

whiteBjörn = theBjörn White
blackBjörn = theBjörn Black
whiteKing = theKing White
blackKing = theKing Black
whitePawns = thePawns White
blackPawns = thePawns Black

-- Check whether a position is valid, meaning:
-- Each player has exactly one björn, at most one king and at most two pawns,
-- no björns or kings are adjacent to each other, no field is occupied twice,
-- no piece has an invalid position and every player has well-specified kingMoves.
-- Use this to validate positions input by the user.
positionValid :: Position -> Bool
positionValid pos = numberOfPieces && nonAdjacent && noMultiOccupation && piecesOnBoard && kingMovesOk where
    numberOfPieces = count Björn White == 1 && count Björn Black == 1 && count King White <= 1 && count King Black <= 1 &&
                     count (Pawn True) White + count (Pawn False) White <= 2 && count (Pawn True) Black + count (Pawn False) Black <= 2 where
        count knd col = length $ filter (pieceMatch knd col) (pieces pos)

    nonAdjacent = distance (square $ whiteBjörn pos) (square $ blackBjörn pos) > 1 &&
                  case (whiteKing pos, blackKing pos) of (Just w, Just b) -> distance (square w) (square b) > 1; _ -> True

    noMultiOccupation = all ((== 1) . length) $ group . sort . (map square) . pieces $ pos
    piecesOnBoard = all (squareValid . square) (pieces pos)
    kingMovesOk = all (\col -> any ((==) col . fst) (kingMoves pos)) [White, Black]