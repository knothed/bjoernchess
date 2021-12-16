module Core.IO (ioTests) where

import Björn.Core.IO
import Björn.Core.Pieces
import Björn.Core.Position
import Control.Monad
import Test.HUnit.Base

ioTests = TestList [
    TestLabel "Square" $ genDefaultTests squareTests parseSquare showSquare,
    TestLabel "PieceKind" $ genDefaultTests pieceKindTests parsePieceKind showPieceKind,
    TestLabel "Piece" $ genDefaultTests pieceTests parsePiece showPiece,
    TestLabel "Position" $ genInverseTests positionTests parsePosition showPosition
  ]

genDefaultTests :: (Eq a, Show a) => [(String, Maybe a)] -> Parser a -> (a -> String) -> Test
genDefaultTests tests parser show = TestList (concatMap mkTest tests) where
    mkTest (str, Nothing) = [parseTest str Nothing parser]
    mkTest (str, Just val) = [parseTest str (Just val) parser, showTest val str show]

genInverseTests :: (Eq a, Show a) => [(String, String -> Maybe String)] -> Parser a -> (a -> String) -> Test
genInverseTests tests parser show = TestList (map mkTest tests) where
    mkTest (str, expected) = parseShowInverseTest str expected parser show
        
parseTest str expected parser = TestCase $ assertEqual ("Parse " ++ brack str) expected (tryParse parser str)
showTest val expected show = TestCase $ assertEqual ("Show " ++ brack expected) expected (show val)
parseShowInverseTest str expected parser show = TestCase $assertEqual ("Show ∘ Parse " ++ brack str) (expected str) (fmap show (tryParse parser str))
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

-- Position IO
-- Attention: These are not safe up to reordering of pieces or special moves
positionTests = [
    ("Kc4,Bd4,bg2;Bk;b", Just),
    ("qf3,pa8;-;w", Just),
    ("Kc4,;kD;b", pure Nothing),
    ("fc4;;b", pure Nothing),
    ("fc4;-;bc3", pure Nothing)
  ]