module Core.IO (ioTests) where

import Björn.Core.IO
import Björn.Core.Pieces
import Björn.Core.Position
import Control.Monad
import Test.HUnit.Base

ioTests = TestList [
    TestLabel "Square" $ genDefaultTests squareTests (tryParse parseSquare) showSquare,
    TestLabel "PieceKind" $ genDefaultTests pieceKindTests (tryParse parsePieceKind) showPieceKind,
    TestLabel "Piece" $ genDefaultTests pieceTests (tryParse parsePiece) showPiece,
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

-- Square IO
squareTests = [
    ("b1", Just (2,1)),
    ("h8", Just (8,8)),
    ("F5", Nothing),
    ("8", Nothing),
    ("a9", Nothing),
    ("bc", Nothing)
  ]

-- PieceKind IO
pieceKindTests = [
    ("K", Just (White, King)),
    ("b", Just (Black, Björn)),
    ("", Nothing),
    ("w", Nothing)
  ]

-- Piece IO
pieceTests = [
    ("Pc6", Just $ mkPiece White (Pawn True) (3,6)),
    ("qb5", Just $ mkPiece Black (Pawn False) (2,5)),
    ("ph0", Nothing),
    ("Öb1", Nothing)
  ] where
    mkPiece color kind square = Piece { color = color, kind = kind, square = square}

-- KingMoves IO
-- Attention: These are not safe up to reordering
kingMovesTests = [
    ("bk", Just [mkMoves White False False, mkMoves Black True True]),
    ("B", Just [mkMoves White False True, mkMoves Black False False]),
    ("Kk", Just [mkMoves White True False, mkMoves Black True False]),
    ("-", Just [mkMoves White False False, mkMoves Black False False]),
    ("", Nothing),
    ("Bw", Nothing)
  ] where
    mkMoves col knight boomerang = (col, KingMoves { hasKnight = knight, hasBoomerang = boomerang })

-- Position IO
-- Attention: These are not safe up to reordering of pieces or special moves
positionTests = [
    ("Kc4,Bd4,bg2;Bk;b", Just),
    ("qf3,pa8;-;w", Just),
    ("Kc4,;kD;b", pure Nothing),
    ("fc4;;b", pure Nothing),
    ("fc4;-;bc3", pure Nothing)
  ]