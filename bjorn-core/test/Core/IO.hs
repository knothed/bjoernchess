module Core.IO (ioTests) where

import Bjorn.Core
import Control.Monad
import Test.HUnit.Base

ioTests = TestList [
    TestLabel "Square" $ genDefaultTests squareTests (tryParse parseSquare) showSquare,
    TestLabel "Piece" $ genDefaultTests pieceTests (tryParse parsePiece) showPiece,
    TestLabel "PieceSq" $ genDefaultTests pieceSqTests (tryParse parsePieceSq) showPieceSq,
    TestLabel "Position" $ genInverseTests positionTests (tryParse parsePosition) showPosition,
    TestLabel "KingMoves" $ genDefaultTests kingMovesTests readKingMoves showKingMoves
  ]

genDefaultTests :: (Eq a, Show a) => [(String, Maybe a)] -> (String -> Maybe a) -> (a -> String) -> Test
genDefaultTests tests read show = TestList (concatMap mkTest tests) where
    mkTest (str, Nothing) = [parseTest str Nothing read]
    mkTest (str, Just val) = [parseTest str (Just val) read, showTest val str show]

genInverseTests :: (Eq a, Show a) => [(String, String -> Maybe String)] -> (String -> Maybe a) -> (a -> String) -> Test
genInverseTests tests read show = TestList (map mkTest tests) where
    mkTest (str, expected) = parseShowInverseTest str expected read show

parseTest str expected read = TestCase $ assertEqual ("Parse " ++ brack str) expected (read str)
showTest val expected show = TestCase $ assertEqual ("Show " ++ brack expected) expected (show val)
parseShowInverseTest str expected read show = TestCase $ assertEqual ("Show ∘ Parse " ++ brack str) (expected str) (fmap show (read str))
brack str = "(" ++ str ++ ")"

squareTests = [
    ("b1", Just (2,1)),
    ("h8", Just (8,8)),
    ("F5", Nothing),
    ("8", Nothing),
    ("a9", Nothing),
    ("bc", Nothing)
  ]

pieceTests = [
    ("K", Just (White, King)),
    ("b", Just (Black, Bjorn)),
    ("", Nothing),
    ("w", Nothing)
  ]

pieceSqTests = [
    ("Pc6", Just (Pawn True, (3,6), White)),
    ("qb5", Just (Pawn False, (2,5), Black)),
    ("ph0", Nothing),
    ("Öb1", Nothing)
  ]

-- Attention: These are not safe up to reordering
kingMovesTests = [
    ("bk", Just [mkMoves White False False, mkMoves Black True True]),
    ("B", Just [mkMoves White False True, mkMoves Black False False]),
    ("Kk", Just [mkMoves White True False, mkMoves Black True False]),
    ("-", Just [mkMoves White False False, mkMoves Black False False]),
    ("", Nothing),
    ("Bw", Nothing)
  ] where
    mkMoves col k b = (col, KingMoves { knight = k, boomerang = b })

-- Attention: These are not safe up to reordering of pieces or special moves
positionTests = [
    ("Kc4,Bd4,bg2;Bk;n;b", Just),
    ("qf3,pa8;-;y;w", Just),
    ("qf3,pa8;-;w", pure Nothing),
    ("Kc4,;kD;n;b", pure Nothing),
    ("fc4;;n;b", pure Nothing),
    ("fc4;-;bc3", pure Nothing)
  ]