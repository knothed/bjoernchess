module Bjorn.Core.Position(
    Position(..), KingMoves(..),
    positionValid
) where

import Bjorn.Core.Pieces
import Bjorn.Core.PosRepr
import Control.Monad (liftM2)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find)

-- An inefficent representation of a position.
data Position = Position {
    pieces :: [(Piece, Square, Color)],
    kingMoves :: [(Color, KingMoves)],
    toMove :: Color,
    pendingKnight :: Bool
} deriving (Eq, Show)

data KingMoves = KingMoves {
    knight :: Bool,
    boomerang :: Bool
} deriving (Eq, Show)

instance PosRepr Position where
    bjorn pos col = snd3 . fromJust $ find (pieceMatch Bjorn col) (pieces pos)
    king pos col = fmap snd3 $ find (pieceMatch King col) (pieces pos)
    pawns pos col = catMaybes $ map (pawnMatch col) (pieces pos)
    occupied pos sq = occupant pos sq /= Nothing
    occupiedBy pos sq col = fmap fst (occupant pos sq) == Just col
    occupant pos sq = fmap (liftM2 (,) thd3 fst3) $ find (squareMatch sq) (pieces pos)
    hasBoomerang pos col = boomerang $ fromJust $ lookup col (kingMoves pos)
    hasKnight pos col = knight $ fromJust $ lookup col (kingMoves pos)
    pendingKnightCheck = pendingKnight
    whoseTurn = toMove

pawnMatch col (pc,sq,col') = case (col == col', pc) of (True, Pawn x) -> Just (sq,x); _ -> Nothing
pieceMatch pc col (pc',_,col') = pc == pc' && col == col'
squareMatch sq (_,sq',_) = sq == sq'

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

-- Check whether a position is valid, meaning:
-- Each player has exactly one bjorn, at most one king and at most two pawns, and the representation is valid.
positionValid :: Position -> Bool
positionValid pos = numberOfPieces && kingMovesOk && posValid pos where
    kingMovesOk = all (\col -> any ((==) col . fst) (kingMoves pos)) [White, Black]
    numberOfPieces = count Bjorn White == 1 && count Bjorn Black == 1 && count King White <= 1 && count King Black <= 1 &&
                     count (Pawn True) White + count (Pawn False) White <= 2 && count (Pawn True) Black + count (Pawn False) Black <= 2 where
        count knd col = length $ filter (pieceMatch knd col) (pieces pos)