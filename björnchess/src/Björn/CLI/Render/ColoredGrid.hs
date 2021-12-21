{-# LANGUAGE ExistentialQuantification #-}

module Björn.CLI.Render.ColoredGrid (
    ColoredGrid(..), Wall(..), allWalls,
    HalfInt(..), integer,
    renderGrid
) where

import Prelude hiding (Left, Right, floor, ceil)
import Control.Applicative (liftA2)
import Control.Monad (ap, mapM)
import Data.Maybe (fromMaybe)
import Data.List (sortBy, groupBy)
import Data.Tuple (swap)
import System.Console.ANSI

-- A grid which is drawn using unicode's box drawing characters.
-- It relies on the font being monospace and having a width:height character ratio of exactly 1:2.
-- Each cell is quadratic with a size of 4x2, and can draw a single colored character in the center.
data ColoredGrid = ColoredGrid {
    width :: Int,
    height :: Int,

    -- Yield the text of the cell at position (x,y), together with the text color (Nothing for default).
    -- The origin is in the upper(!) left corner.
    cells :: Int -> Int -> Maybe (Char, Maybe Color),

    -- Yield the set of walls a cell at position (x,y) has.
    -- The origin is in the upper(!) left corner.
    -- If a cell (x,y) has a left wall, then the cell (x-1,y) must have a right wall and vice versa.
    -- If a cell (x,y) has a top wall, then the cell (x,y-1) must have a bottom wall and vice versa.
    wallsForCell :: Int -> Int -> [Wall],

    -- Provide a text that shall be printed at the right side of the grid.
    -- Full integers are used to enumerate the cells (starting at 0),
    -- while half integers are used to enumerate the walls between the cells (starting at -0.5).
    trailingTextForRow :: HalfInt -> Maybe String
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

-- A value in 0.5 * Z.
data HalfInt = HalfInt Int deriving (Eq, Ord, Show) -- Twice the value
integer n = HalfInt (2*n)
integral (HalfInt x) = even x
just = floor
floor (HalfInt x) = div x 2
ceil (HalfInt x) = div (x+1) 2

-- Draw the grid onto the terminal.
renderGrid :: ColoredGrid -> IO ()
renderGrid grid = do mapM_ draw sortedDrawables where
    sortedDrawables = (map head . groupBy eqPos . sortBy cmpPos) (innerStuff ++ trailingRows)
    innerStuff = concatMap drawablesForCell $ cartProd [0 .. width grid - 1] [0 .. height grid - 1]
    trailingRows = map (AnyDrawable . ap Trailing (trailingTextForRow grid) . HalfInt) [-1 .. 2 * height grid - 1]

    drawablesForCell (x, y) = [
        AnyDrawable $ Cell (integer x) (integer y) (cells grid x y),
        AnyDrawable $ Line (HalfInt (2*x-1)) (integer y) (wallsForCell grid),
        AnyDrawable $ Line (HalfInt (2*x+1)) (integer y) (wallsForCell grid),
        AnyDrawable $ Line (integer x) (HalfInt (2*y-1)) (wallsForCell grid),
        AnyDrawable $ Line (integer x) (HalfInt (2*y+1)) (wallsForCell grid),
        AnyDrawable $ Corner (HalfInt (2*x-1)) (HalfInt (2*y-1)) (wallsForCell grid),
        AnyDrawable $ Corner (HalfInt (2*x-1)) (HalfInt (2*y+1)) (wallsForCell grid),
        AnyDrawable $ Corner (HalfInt (2*x+1)) (HalfInt (2*y-1)) (wallsForCell grid),
        AnyDrawable $ Corner (HalfInt (2*x+1)) (HalfInt (2*y+1)) (wallsForCell grid)
      ]

    eqPos a b = position a == position b
    cmpPos a b = compare (swap (position a)) (swap (position b))
    cartProd = liftA2 (,)

---- Drawing corners, lines and cells

class Drawable a where
    position :: a -> (HalfInt, HalfInt) -- (x,y) position
    draw :: a -> IO () -- print something to the terminal

-- Existential type allowing lists of Drawables
data AnyDrawable = forall a. Drawable a => AnyDrawable a
instance Drawable AnyDrawable where
    position (AnyDrawable a) = position a
    draw (AnyDrawable a) = draw a

data Cell = Cell HalfInt HalfInt (Maybe (Char, Maybe Color)) -- x, y, (content, color)
instance Drawable Cell where
    position (Cell x y _) = (x, y)
    draw (Cell _ _ (Just (char, col))) = putStrCol col [' ', char, ' ']
    draw (Cell _ _ Nothing) = putStrCol Nothing [' ', ' ', ' ']

data Line = Line HalfInt HalfInt (Int -> Int -> [Wall]) -- (x, y) center, wallsForCell
instance Drawable Line where
    position (Line x y _) = (x, y)
    draw line = putStrCol Nothing $ replicate amount str where
        amount = if horizontal line then 3 else 1
        str = if not (visible line) then ' ' else (if horizontal line then unicodeHorizontalLine else unicodeVerticalLine)

data Corner = Corner HalfInt HalfInt (Int -> Int -> [Wall]) -- (x, y) position, wallsForCell
instance Drawable Corner where
    position (Corner x y _) = (x, y)
    draw corner = putStrCol Nothing $ (return . unicodeCorner . wallsAtCorner) corner

data Trailing = Trailing HalfInt (Maybe String) -- half-row, trailing text
instance Drawable Trailing where
    position (Trailing y _) = (HalfInt maxBound, y)
    draw (Trailing _ str) = putStrCol Nothing $ " " ++ (fromMaybe "" str) ++ "\n"

horizontal line@(Line x y walls)
  | integral x && not (integral y) = True
  | integral y && not (integral x) = False
  | otherwise = error $ "Illegal line: " ++ show (x,y)

visible line@(Line x y walls)
   | horizontal line = single (elem Top $ walls (just x) (ceil y)) (elem Bottom $ walls (just x) (floor y))
   | otherwise       = single (elem Right $ walls (floor x) (just y)) (elem Left $ walls (ceil x) (just y))
  where
      single a b = if a == b then a else error $ "Illegal wall specification at: " ++ show (x,y)

wallsAtCorner (Corner x y walls) = [Top | top] ++ [Bottom | bottom] ++ [Left | left] ++ [Right | right] where
    top = visible (Line x (integer . floor $ y) walls)
    bottom = visible (Line x (integer . ceil $ y) walls)
    left = visible (Line (integer . floor $ x) y walls)
    right = visible (Line (integer . ceil $ x) y walls)

---- Unicode characters

unicodeHorizontalLine = unicodeCorner [Left, Right]
unicodeVerticalLine = unicodeCorner [Top, Bottom]

unicodeCorner walls = [' ', '╺', '╸', '━', '╻', '┏', '┓', '┳', '╹', '┗', '┛', '┻', '┃', '┣', '┫', '╋'] !! idx where
--unicodeCorner walls = [' ', '╶', '╴', '─', '╷', '┌', '┐', '┬', '╵', '└', '┘', '┴', '│', '├', '┤', '┼'] !! idx where
    idx = 8 * fromEnum (elem Top walls) + 4 * fromEnum (elem Bottom walls) + 2 * fromEnum (elem Left walls) + 1 * fromEnum (elem Right walls)

---- Helpers

putStrCol :: Maybe Color -> String -> IO ()
putStrCol Nothing a = putStr a
putStrCol (Just col) str = do
    setSGR [ SetColor Foreground Dull col ]
    putStr str
    setSGR [ Reset ]