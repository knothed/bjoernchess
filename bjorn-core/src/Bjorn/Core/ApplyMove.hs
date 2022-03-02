module Bjorn.Core.ApplyMove (
    moveValid, moveWins, stalemate, applyMove, givesPawnCheck
) where

import Bjorn.Core.MoveGen
import Bjorn.Core.Pieces
import Bjorn.Core.PosRepr
import Bjorn.Core.Position
import Bjorn.Core.Utils
import Data.Maybe
import Data.List

-- Determine whether a move can be applied to the given position.
moveValid :: PosRepr a => a -> Move -> Bool
moveValid pos move = elem move (genMoves pos)

-- Determine whether a move wins the game for the moving player. The move is assumed to be valid.
moveWins :: PosRepr a => a -> Move -> Bool
moveWins pos move = bjornBeaten || bjornPromoted || twoKings where
    player = whoseTurn pos
    promotionRank = if player == Black then 1 else boardSize
    bjornBeaten = occupant pos (dest move) == Just (opp player, Bjorn)
    bjornPromoted = piece move == Bjorn && snd (dest move) == promotionRank
    twoKings = isPawn (piece move) && snd (dest move) == promotionRank && isJust (king pos player)

stalemate :: Position -> Bool
stalemate = null . genMoves

-- Apply a move to a position. The move is assumed to be valid.
applyMove :: Position -> Move -> Position
applyMove pos move = Position { pieces = newPieces, kingMoves = newKingMoves, toMove = opp player, pendingKnight = newPendingKnight } where
    player = toMove pos
    
    newPieces = promote movedPiece : (removeKing . removeSrcDest) (pieces pos)
    movedPiece = (pawnMod (piece move), dest move, player)
    removeSrcDest = filter (not . flip elem [src move, dest move] . snd3)
    removeKing = if givesPawnCheck pos move || knightCheck move then filter (\(pc, _, col) -> pc == King && col == opp player) else id

    promotionRank = if player == Black then 1 else boardSize
    promote (pc, sq, col) = if isPawn pc && (snd sq == promotionRank) then (King, sq, col) else (pc, sq, col)
    pawnMod (Pawn True) = Pawn (moveType move /= Double)
    pawnMod x = x

    newKingMoves = [(player, modify $ lookupJust player (kingMoves pos)), (opp player, lookupJust (opp player) (kingMoves pos))]
    modify moves = KingMoves { knight = knight moves && moveType move /= Knight, boomerang = boomerang moves && moveType move /= Boomerang }
    newPendingKnight = knightCheck move && hasKnight pos (opp player)

givesPawnCheck :: PosRepr a => a -> Move -> Bool
givesPawnCheck pos move = isPawn (piece move) && any (flip elem [(x-1,y+k), (x+1,y+k)]) (king pos (opp player)) where
    player = whoseTurn pos
    k = mvmtDir player
    (x,y) = dest move