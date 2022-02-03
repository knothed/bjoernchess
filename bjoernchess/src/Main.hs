module Main where

import Björn.Core
import Björn.CLI.GUI.RenderBoard
import Data.Maybe
import System.Console.ANSI (xterm24LevelGray, xterm6LevelRGB)

startPos = fromJust $ tryParse parsePosition "Kf2,Be2,kd8,bd7,pc7,pe7;KBkb;n;w"
coloring = [(Color White, xterm6LevelRGB 5 2 0), (Color Black, xterm6LevelRGB 1 1 5), (Checker, xterm24LevelGray 20)]

main :: IO ()
main = do
    renderBoard startPos coloring
    let m = map dest . filter ((==) Knight . moveType) $ genMoves startPos
    print m