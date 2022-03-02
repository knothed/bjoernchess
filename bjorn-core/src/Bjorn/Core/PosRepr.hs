module Bjorn.Core.PosRepr where

import Bjorn.Core.Pieces
import Data.Foldable (toList)
import Data.List (find, group, sort)

class PosRepr a where
    bjorn :: a -> Color -> Square
    king :: a -> Color -> Maybe Square
    pawns :: a -> Color -> [(Square, Bool)]
    occupied :: a -> Square -> Bool
    occupiedBy :: a -> Square -> Color -> Bool
    occupant :: a -> Square -> Maybe (Color, Piece)
    hasBoomerang :: a -> Color -> Bool
    hasKnight :: a -> Color -> Bool
    pendingKnightCheck :: a -> Bool
    whoseTurn :: a -> Color

allPieces :: PosRepr a => a -> [(Piece, Square, Color)]
allPieces pos = pcs White ++ pcs Black where
    pcs col = pc Bjorn [bjorn pos col] ++ pc King (toList (king pos col)) ++ map pawn (pawns pos col) where
        pc p pcs = map (\sq -> (p, sq, col)) pcs
        pawn (sq, b) = (Pawn b, sq, col)

-- Check whether a position representation is valid, meaning:
-- No bjorns or kings are adjacent to each other, no field is occupied twice and no piece has an invalid position.
-- Use this to validate positions input by the user.
posValid :: PosRepr a => a -> Bool
posValid pos = nonAdjacent && noMultiOccupation && piecesOnBoard where
    nonAdjacent = dist (bjorn pos White) (bjorn pos Black) > 1 &&
                  case (king pos White, king pos Black) of (Just w, Just b) -> dist w b > 1; _ -> True
    noMultiOccupation = all ((== 1) . length) $ group . sort . (map square) . allPieces $ pos
    piecesOnBoard = all (validSquare . square) (allPieces pos)
    square (_,sq,_) = sq