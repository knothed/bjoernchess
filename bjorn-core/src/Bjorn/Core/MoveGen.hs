module Bjorn.Core.MoveGen (
    Move(..), MoveType(..), genMoves, genPawnMoves
) where

import Bjorn.Core.Pieces
import Bjorn.Core.PosRepr
import Data.Maybe
import Data.List

data Move = Move {
    piece :: Piece,
    src :: Square,
    dest :: Square,
    moveType :: MoveType,
    knightCheck :: Bool -- Player gives or returns an earlier knight check
} deriving (Eq, Show)

data MoveType = Normal | Boomerang | Knight | Double deriving (Eq, Show)

mkMove pc src dest mtype knight = Move { piece = pc, src = src, dest = dest, moveType = mtype, knightCheck = knight }

----- MOVE-GEN -----

genMoves :: PosRepr a => a -> [Move]
genMoves pos = allMoves ++ if pendingKnightCheck pos then map returnKnightCheck allMoves else [] where
    allMoves = genBjornMoves pos ++ genPawnMoves pos ++ genKingMoves pos
    returnKnightCheck mv = mkMove (piece mv) (src mv) (dest mv) (moveType mv) True

genBjornMoves :: PosRepr a => a -> [Move]
genBjornMoves pos = map move $ filter valid $ neighbors ourB where
    col = whoseTurn pos
    ourB = bjorn pos col
    theirB = bjorn pos (opp col)
    valid sq = dist sq theirB > 1 && all canBeat (occupant pos sq)
    canBeat (col', pc) = col' /= col && isPawn pc
    move sq = mkMove Bjorn ourB sq Normal False

genPawnMoves :: PosRepr a => a -> [Move]
genPawnMoves pos = concatMap pawnMoves (pawns pos col) where
    col = whoseTurn pos
    pawnMoves (sq, double) = catMaybes (map (move sq double) (neighbors sq)) ++
                             if double then catMaybes (map (uncurry $ doubleMove sq double) (doubleNeighbors sq)) else [] where

    neighbors (x,y) = filter validSquare [(x+1,y), (x-1,y), (x,y+k)] where k = mvmtDir col
    doubleNeighbors (x,y) = filter (validSquare . fst) [((x+2,y), Just (x+1,y)), ((x-2,y), Just (x-1,y)), ((x,y+2*k), Just (x,y+k)), ((x+1,y+k), Nothing), ((x-1,y+k), Nothing)]
                            where k = mvmtDir col

    move from d to = if not (occupied pos to) then move else Nothing
        where move = Just $ mkMove (Pawn d) from to Normal False

    doubleMove from d to via = case fmap fst (occupant pos to) of
        Just col' -> if via == Nothing && col /= col' then move else Nothing
        Nothing -> if not $ any (occupied pos) via then move else Nothing
        where move = Just $ mkMove (Pawn d) from to Double False

genKingMoves :: PosRepr a => a -> [Move]
genKingMoves pos = onlyWhen True ourK genNormalMoves ++ onlyWhen (hasKnight pos col) ourK genKnightMoves ++ onlyWhen (hasBoomerang pos col) ourK genBoomerangMoves where
    onlyWhen what king trans = if what then maybe [] trans king else []
    col = whoseTurn pos
    ourK = king pos col
    justK = fromJust ourK
    theirK = king pos (opp col)
    valid sq = all ((> 1) . dist sq) theirK && not (elem sq inCheck) && not (occupiedBy pos sq col)
    inCheck = concatMap (\(x,y) -> [(x-1,y-k), (x+1,y-k)]) . map fst $ pawns pos (opp col) where k = mvmtDir col
    -- inCheck (x,y) = any (flip elem [(x-1,y+k), (x+1,y+k)] . fst) $ pawns pos (opp col) where k = mvmtDir col

    genNormalMoves = concatMap moves . filter valid . neighbors where
        moves sq = [mkMove King justK sq Normal False] ++ [mkMove King justK sq Normal True | any (knightAway justK) theirK && not (pendingKnightCheck pos)]

    genKnightMoves = map move . filter valid . knights where
        move sq = mkMove King justK sq Knight False

    genBoomerangMoves sq = map move . rmDups . filter ((>= 2) . dist sq) . diagonals $ sq where
        move sq = mkMove King justK sq Boomerang False
        rmDups = map head . group . sort

        diagonals :: Square -> [Square]
        diagonals (x,y) = case (elem x [1, boardSize], elem y [1, boardSize]) of
            (False, False) -> runBothDirs (x,y) (1,1) ++ runBothDirs (x,y) (1,-1) -- all 4 directions
            (False, True) -> runBothDirs (x,y) (1,1) -- 2 directions
            (True, False) -> runBothDirs (x,y) (1,1) -- 2 directions
            (True, True) -> runBothDirs (x,y) dir where -- 1 direction, mustn't be into the corner
              dir = if x == 1 then (1,1) else (-1,1)

        runBothDirs :: Square -> (Int, Int) -> [Square]
        runBothDirs sq (dx,dy)
          | reachedStart = fstRun
          | otherwise = fstRun ++ sndRun
          where
            (fstRun, reachedStart) = follow sq (dx,dy) sq True
            (sndRun, _) = follow sq (-dx,-dy) sq True
        
        follow (x,y) (dx,dy) start first = case (out x, out y) of
            (True, True) -> ([], False)
            (True, False) -> follow (flip x, y) (-dx, dy) start first
            (False, True) -> follow (x, flip y) (dx, -dy) start first
            (False, False) -> continue
          where
            out = not . between 1 boardSize
            between a b x = a <= x && x <= b
            flip x = if x < 1 then 2-x else 2*boardSize-x
            
            continue
              | first = follow (x+dx,y+dy) (dx,dy) start False -- don't add the start square
              | (x,y) == start = ([], True)
              | not (valid (x,y)) = ([], False)
              | occupiedBy pos (x,y) (opp col) = ([(x,y)], False)
              | otherwise = let (a,b) = follow (x+dx,y+dy) (dx,dy) start False in ((x,y):a, b)


-- All onboard squares with distance 1.
neighbors :: Square -> [Square]
neighbors (x,y) = filter validSquare [(x-1,y), (x+1,y), (x,y-1), (x,y+1), (x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

-- All onboard squares a knight jump away.
knights :: Square -> [Square]
knights (x,y) = filter validSquare [(x-1,y-2), (x-1,y+2), (x+1,y-2), (x+1,y+2), (x-2,y-1), (x-2,y+1), (x+2,y-1), (x+2,y+1)]