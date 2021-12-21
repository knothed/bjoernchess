module Main where

import Björn.Core.Pieces
import Björn.Core.IO
import Björn.CLI.Render.Position
import Data.Maybe
import qualified System.Console.ANSI

startPos = fromJust $ tryParse parsePosition "Ke1,Be2,Pd2,Pf2,kd8,bd7,pc7,pe7;KBkb;w"
coloring = [(White, System.Console.ANSI.Red), (Black, System.Console.ANSI.Cyan)]

main :: IO ()
main = do
    renderPosition startPos coloring