module Björn.CLI.Render.Position (renderPosition) where

import Björn.Core.Pieces
import Björn.Core.Position
import Björn.Core.IO
import Björn.CLI.Render.ColoredGrid
import Data.List
import Data.Maybe
import qualified System.Console.ANSI

-- Displays a position in the terminal in a square grid using ANSI coloring.
-- Requires a monospace font in the terminal.
renderPosition :: Position -> [(Björn.Core.Pieces.Color, System.Console.ANSI.Color)] -> IO ()
renderPosition position coloring = renderGrid grid where
    grid = ColoredGrid {
      width = boardSize + 2,
      height = boardSize + 2,
      cells = flipY (boardSize + 1) cells,
      wallsForCell = shiftWalls (-1) (-1) (allWalls boardSize boardSize),
      trailingTextForRow = flip lookup [(HalfInt 7, whoseTurn), (HalfInt 9, kingMovesText White), (HalfInt 10, kingMovesText Black)]
    }

    flipY max f x y = f x (max - y)
    shiftWalls dx dy walls x y = walls (x + dx) (y + dy)

    -- Cell content
    cells x y
      | (x == 0 || x == boardSize + 1) && (y == 0 || y == boardSize + 1) = Nothing
      | (x == 0 || x == boardSize + 1) = Just (last $ showSquare (1, y), Nothing)
      | (y == 0 || y == boardSize + 1) = Just (head $ showSquare (x, 1), Nothing)
      | otherwise = maybe Nothing draw $ pieceAt x y
    pieceAt x y = find ((== (x,y)) . square) (pieces position)
    draw piece = Just (fromJust $ lookup (color piece, kind piece) pieceChars, lookup (color piece) coloring)

    pieceChars = [
        ((White, Pawn True), '*'),  ((Black, Pawn True), '*'),
        ((White, Pawn False), '•'), ((Black, Pawn False), '•'),
        ((White, King), '†'),       ((Black, King), '†'),
        ((White, Björn), '▲'),      ((Black, Björn), '▼')
      ]
    
    -- Text for player to move and special moves
    whoseTurn = show (toMove position) ++ "'s turn"
    kingMovesText col = show col ++ " has " ++ (ifEmpty "none" . kingMovesVals . fromJust . lookup col . kingMoves) position
    kingMovesVals moves = intercalate " + " (["B" | hasBoomerang moves] ++ ["K" | hasKnight moves])
    ifEmpty a b = if null b then a else b