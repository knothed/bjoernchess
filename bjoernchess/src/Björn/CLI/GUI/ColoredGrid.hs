module Björn.CLI.GUI.ColoredGrid (ColoredGrid(..), Wall(..), allWalls, renderGrid) where

import Prelude hiding (Left, Right)
import Control.Monad (mapM, when)
import Data.Maybe (fromMaybe)
import Data.Word
import System.Console.ANSI

-- A grid which is drawn using unicode's box drawing characters.
-- It relies on the font being monospace and having a width:height character ratio of exactly 1:2.
-- Each cell is quadratic with a size of 4x2, and can draw a single colored character in the center.
data ColoredGrid = ColoredGrid {
    width :: Int,
    height :: Int,

    -- Yield the text of the cell at position (x,y), together with the palette text color (Nothing for default).
    -- The origin is in the upper(!) left corner.
    cells :: Int -> Int -> Maybe (Char, Maybe Word8),

    -- Yield the set of walls a cell at position (x,y) has.
    -- The origin is in the upper(!) left corner.
    -- If a cell (x,y) has a left wall, then the cell (x-1,y) must have a right wall and vice versa.
    -- If a cell (x,y) has a top wall, then the cell (x,y-1) must have a bottom wall and vice versa.
    walls :: Int -> Int -> [Wall],

    -- Provide a text that shall be printed at the right side of the grid.
    -- The range is [0, 2*height]. Even numbers represent rows made of horizontal walls
    -- while odd numbers represent rows made of cells and vertical walls.
    trailingTexts :: Int -> Maybe String
}

-- The directions where a wall can be.
-- This can be used BOTH from the perspective of a cell (where up & down mean horizontal lines),
-- AND from the the perspective of a corner (where up & down mean vertical lines.)
data Wall = Top | Bottom | Left | Right deriving (Eq)

-- Draw all possible walls for a grid of size w x h.
allWalls w h x y =
    [Top | between 0 h y && between 0 (w-1) x ] ++ [Bottom | between (-1) (h-1) y && between 0 (w-1) x ] ++
    [Left | between 0 w x && between 0 (h-1) y ] ++ [Right | between (-1) (w-1) x && between 0 (h-1) y ] where
    between a b x = a <= x && x <= b

-- Draw the grid onto the terminal.
renderGrid :: ColoredGrid -> IO ()
renderGrid grid = mapM_ drawRow [0 .. height grid - 1] where
    -- Recursive drawing
    drawRow y = do
        when (y == 0) $ drawWallRow y
        drawCellRow y
        drawWallRow (y+1)
        
    drawWallRow y = do
        mapM_ (drawWallAndCorners y) [0 .. width grid - 1]
        drawTrailing (2*y)

    drawWallAndCorners y x = do
        when (x == 0) $ drawCorner x y
        drawHorizWall x y
        drawCorner (x+1) y

    drawCellRow y = do
        mapM_ (drawCellAndWalls y) [0 .. width grid - 1]
        drawTrailing (2*y+1)

    drawCellAndWalls y x = do
        when (x == 0) $ drawVertWall x y
        drawCell x y
        drawVertWall (x+1) y
    
    -- Content
    drawCell x y = case cells grid x y of
        Just (char, col) -> putStrCol col [' ', char, ' ']
        Nothing -> putStrCol Nothing [' ', ' ', ' ']

    drawHorizWall x y = putStrCol Nothing $ replicate 3 (if visibleHorizLine x y then unicodeHorizLine else ' ')
    drawVertWall x y = putStrCol Nothing $ replicate 1 (if visibleVertLine x y then unicodeVertLine else ' ')
    
    drawCorner x y = putStrCol Nothing $ [unicodeCorner (wallsAtCorner x y)]
    drawTrailing y = putStrCol Nothing $ " " ++ fromMaybe "" (trailingTexts grid y) ++ "\n"

    visibleHorizLine x y = single (elem Top $ walls grid x y) (elem Bottom $ walls grid x (y-1)) where
        single a b = if a == b then a else error $ "Illegal horizontal wall specification at " ++ show (x,y) ++ " and " ++ show (x,y-1)
    visibleVertLine x y = single (elem Left $ walls grid x y) (elem Right $ walls grid (x-1) y) where
        single a b = if a == b then a else error $ "Illegal vertical wall specification at " ++ show (x,y) ++ " and " ++ show (x-1,y)

    wallsAtCorner :: Int -> Int -> [Wall]
    wallsAtCorner x y =
        [Top | visibleVertLine x (y-1)] ++ [Bottom | visibleVertLine x y] ++ [Left | visibleHorizLine (x-1) y] ++ [Right | visibleHorizLine x y]

-- Unicode chars
unicodeCorner walls = [' ', '╺', '╸', '━', '╻', '┏', '┓', '┳', '╹', '┗', '┛', '┻', '┃', '┣', '┫', '╋'] !! idx where
-- unicodeCorner walls = [' ', '╶', '╴', '─', '╷', '┌', '┐', '┬', '╵', '└', '┘', '┴', '│', '├', '┤', '┼'] !! idx where // Thin characters
    idx = 8 * fromEnum (elem Top walls) + 4 * fromEnum (elem Bottom walls) + 2 * fromEnum (elem Left walls) + 1 * fromEnum (elem Right walls)
    
unicodeHorizLine = unicodeCorner [Left, Right]
unicodeVertLine = unicodeCorner [Top, Bottom]

-- Helpers
putStrCol :: Maybe Word8 -> String -> IO ()
putStrCol Nothing a = putStr a
putStrCol (Just col) str = do
    setSGR [ SetPaletteColor Foreground col ]
    putStr str
    setSGR [ Reset ]