module Main where

import Bjorn.Core
import Bjorn.CLI.GUI.RenderBoard
import Data.Maybe
import Data.List
import System.Console.ANSI (xterm24LevelGray, xterm6LevelRGB)

startPos = fromJust $ tryParse parsePosition "Ke1,Be2,Pd2,Pf2,kd8,bd7,pc7,pe7;KBkb;n;w"
coloring = [(Color White, xterm6LevelRGB 5 2 0), (Color Black, xterm6LevelRGB 1 1 5), (Checker, xterm24LevelGray 20)]

main :: IO ()
main = do
    print startPos
    renderBoard startPos coloring
    let m = map (showMove startPos) (genMoves startPos)
    putStrLn (intercalate "; " m)

    let b = map (parseMove startPos) ["Kge3"]
    print b