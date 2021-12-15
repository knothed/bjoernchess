module Main where

import Björn.Core.IO
import Data.Maybe

main :: IO ()
main = do
    let a = fromJust $ tryParse parsePosition "Ke1,Öe2,Pd2,Pf2,kd8,öd7,pc7,pf7;-;b"
    putStrLn $ showPosition a