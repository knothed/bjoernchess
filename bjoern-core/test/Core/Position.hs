module Core.Position (positionTests) where

import Björn.Core.IO
import Björn.Core.Pieces
import Björn.Core.Position
import Data.Maybe
import Test.HUnit.Base

positionTests = TestList (map testCase testCases)

testCase (pos, valid) = TestCase $ assertEqual ("Valid " ++ brack pos) valid (positionValid . fromJust $ tryParse parsePosition pos)
brack str = "(" ++ str ++ ")"

testCases = [
    ("Kc2,bc4;-;w", False), -- no björn
    ("Kc2,Kf3,Bc4,bd1;-;w", False), -- 2 kings
    ("Ba1,ba2;-;w", False), -- adjacent björns
    ("Ba1,ba8,Kc1,kd2;-;w", False), -- adjacent kings
    ("Ba1,ba5,pc3,qc3;-;w", False), -- multi-occupation
    ("Ba1,ba5,pc3,Kc3;-;w", False), -- multi-occupation
    ("Kc4,Bd4,bg2;-;w", True),
    ("Ke1,Be2,Pd2,Pf2,kd8,bd7,pc7,pe7;-;w", True)
  ]