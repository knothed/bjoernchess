module Björn.CLI.GUI.RenderBoard (renderPosition) where

import Björn.Core
import Björn.CLI.GUI.ColoredGrid
import Data.List
import Data.Maybe
import qualified System.Console.ANSI

-- Displays a position in the terminal in a square grid using ANSI coloring.
-- Requires a monospace font in the terminal.
renderPosition :: PosRepr a => a -> [(Björn.Core.Color, System.Console.ANSI.Color)] -> IO ()
renderPosition pos coloring = renderGrid grid where
    grid = ColoredGrid {
      width = boardSize + 2,
      height = boardSize + 2,
      cells = flipY (boardSize + 1) cells,
      walls = shiftWalls (-1) (-1) (allWalls boardSize boardSize),
      trailingTexts = flip lookup [(8, turn), (10, kingMovesText White), (11, kingMovesText Black)]
    }

    flipY max f x y = f x (max - y)
    shiftWalls dx dy walls x y = walls (x + dx) (y + dy)

    -- Cell content
    cells x y
      | (x == 0 || x == boardSize + 1) && (y == 0 || y == boardSize + 1) = Nothing
      | (x == 0 || x == boardSize + 1) = Just (last $ showSquare (1, y), Nothing)
      | (y == 0 || y == boardSize + 1) = Just (head $ showSquare (x, 1), Nothing)
      | otherwise = maybe Nothing draw $ occupant pos (x,y)
    draw (col, pc) = Just (fromJust $ lookup (col,pc) pieceChars, lookup col coloring)

    pieceChars = [
        ((White, Pawn True), '*'),  ((Black, Pawn True), '*'),
        ((White, Pawn False), '•'), ((Black, Pawn False), '•'),
        ((White, King), '†'),       ((Black, King), '†'),
        ((White, Björn), '▲'),      ((Black, Björn), '▼')
      ]
    
    -- Text for player to move and special moves
    turn = show (whoseTurn pos) ++ "'s turn"
    kingMovesText col = show col ++ " has " ++ ifEmpty "none" (kingMovesVals col)
    kingMovesVals col = intercalate " + " (["B" | hasBoomerang pos col] ++ ["K" | hasKnight pos col])
    ifEmpty a b = if null b then a else b