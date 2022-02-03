module Björn.CLI.GUI.RenderBoard (BoardColor(..), renderBoard) where

import Björn.Core
import Björn.CLI.GUI.ColoredGrid
import Data.List
import Data.Maybe
import Data.Word

data BoardColor = Color Color | Checker deriving (Eq)

-- Displays a position in the terminal in a square grid using ANSI coloring.
-- Requires a monospace font in the terminal.
renderBoard :: PosRepr a => a -> [(BoardColor, Word8)] -> IO ()
renderBoard pos coloring = renderGrid grid where
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
      | otherwise = maybe evenOdd draw $ occupant pos (x,y) where
        evenOdd = if even (x+y) then Just (checkerboardChar, lookup Checker coloring) else Nothing
        draw (col, pc) = Just (fromJust $ lookup (col,pc) pieceChars, lookup (Color col) coloring)

    checkerboardChar = '·'
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