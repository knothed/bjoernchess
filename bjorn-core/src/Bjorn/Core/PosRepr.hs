module Bjorn.Core.PosRepr where

import Bjorn.Core.Pieces
import Bjorn.Core.Utils
import Data.Foldable (toList)
import Data.List (find, group, sort)
import Data.Maybe

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
-- Pawns may not be on the outermost ranks and bjorns not on the winning rank.
-- A pending knight check must be valid.
-- Use this to validate positions input by the user.
posValid :: PosRepr a => a -> Bool
posValid pos = nonAdjacent && noMultiOccupation && piecesOnBoard && ranksValid1 && ranksValid2 && knightCheckValid where
    nonAdjacent = dist (bjorn pos White) (bjorn pos Black) > 1 &&
                  case (king pos White, king pos Black) of (Just w, Just b) -> dist w b > 1; _ -> True
    noMultiOccupation = all ((== 1) . length) $ group . sort . (map snd3) . allPieces $ pos
    piecesOnBoard = all (validSquare . snd3) (allPieces pos)
    ranksValid1 = not (snd (bjorn pos White) == boardSize || fst (bjorn pos Black) == 1)
    ranksValid2 = all (flip notElem [1, boardSize] . snd . fst) (pawns pos White ++ pawns pos Black)
    knightCheckValid = not (pendingKnightCheck pos) || (hasKnight pos player && not (hasKnight pos (opp player)) && isJust (king pos (opp player)) && isNothing (king pos player)) where player = whoseTurn pos

knightAway :: Square -> Square -> Bool
knightAway (x1,y1) (x2,y2) = elem (abs (x1-y1), abs (x2-y2)) [(1,2), (2,1)]