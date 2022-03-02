module Core.Position (positionTests) where

import Bjorn.Core
import Data.Maybe
import Test.HUnit.Base

positionTests = TestList (map testCase testCases)

testCase (pos, valid) = TestCase $ assertEqual ("Valid " ++ brack pos) valid (positionValid . fromJust $ tryParse parsePosition pos)
brack str = "(" ++ str ++ ")"

testCases = [
    ("Kc2,bc4;-;n;w", False), -- no bjorn
    ("Kc2,Kf3,Bc4,bd1;-;n;w", False), -- 2 kings
    ("Ba1,ba2;-;n;w", False), -- adjacent bjorns
    ("Ba1,ba8,Kc1,kd2;-;n;w", False), -- adjacent kings
    ("Ba1,ba5,pc3,qc3;-;n;w", False), -- multi-occupation
    ("Ba1,ba5,pc3,Kc3;-;n;w", False), -- multi-occupation
    ("Ba8,ba2;-;n;w", False), -- bjorn on winning rank
    ("Ba1,ba8,pc1;-;n;w", False), -- pawn on 1. rank
    ("Ba1,ba8,pc8;-;n;w", False), -- pawn on 8. rank
    ("Kb4,Ba3,Bb7;K;y;w", False), -- invalid pending knight check
    ("kb4,Ba3,Bb7;k;y;w", False), -- invalid pending knight check
    ("Kc4,Bd4,bg8;-;n;w", True),
    ("kc2,Ba3,bb7;K;y;w", True),
    ("Ke1,Be2,Pd2,Pf2,kd8,bd7,pc7,pe7;-;n;w", True)
  ]