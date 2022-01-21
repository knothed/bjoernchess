module Björn.Core.IO (
    Parser, tryParse,
    
    showSquare, parseSquare,
    showPiece, parsePiece,
    showPieceSq, parsePieceSq,
    showKingMoves, readKingMoves,
    showPosition, parsePosition
) where

-- Björn.IO provides in- and output of various game-related data types like square, piece and position.

import Björn.Core.Pieces
import Björn.Core.Position
import Control.Monad ((<=<), ap)
import Control.Monad.Trans (lift)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Maybe
import Data.Tuple (swap)
import Text.Parsec

type Parser a = ParsecT String () Maybe a

tryParse :: Parser a -> String -> Maybe a
tryParse p = either (const Nothing) Just <=< runParserT p () ""

---- Square IO ("a8")
showSquare :: Square -> String
showSquare (x, y) = (chr (ord 'a' - 1 + x)) : show y

parseSquare :: Parser Square
parseSquare = do
    x <- validate =<< (\c -> ord c - ord 'a' + 1) <$> letter
    y <- validate =<< read . pure <$> digit
    return (x, y) where
        validate i = if (1 <= i && i <= boardSize) then return i else lift Nothing

---- Piece IO ("K")
showPiece :: (Color, Piece) -> String
showPiece = pure . flip lookupJust pieceTable

parsePiece :: Parser (Color, Piece)
parsePiece = lift =<< flip reverseLookup pieceTable <$> letter

pieceTable = [
    ((White, Pawn True), 'P'),  ((Black, Pawn True), 'p'),
    ((White, Pawn False), 'Q'), ((Black, Pawn False), 'q'),
    ((White, King), 'K'),       ((Black, King), 'k'),
    ((White, Björn), 'B'),      ((Black, Björn), 'b')
  ]

---- PieceSq IO ("pb3")
showPieceSq :: (Piece, Square, Color) -> String
showPieceSq (pc, sq, col) = showPiece (col, pc) ++ showSquare sq

parsePieceSq :: Parser (Piece, Square, Color)
parsePieceSq = do
    (col, pc) <- parsePiece
    sq <- parseSquare
    return (pc, sq, col)

---- Position IO ("Ke1,Be2,Pd2,Pf2,kd8,bd7,pc7,pf7;BKbk;y;w")
showPosition :: Position -> String
showPosition pos = intercalate [blockSep] [pcs, king, pure knight, pure move] where
    pcs = intercalate [pieceSep] $ map showPieceSq $ pieces pos
    king = showKingMoves (kingMoves pos)
    knight = lookupJust (pendingKnight pos) boolTable
    move = lookupJust (toMove pos) colorTable

parsePosition :: Parser Position
parsePosition = do
    pieces <- sepBy parsePieceSq (char pieceSep)
    char blockSep
    kingMoves <- lift . readKingMoves =<< many (satisfy (/= blockSep))
    char blockSep
    knight <- lift =<< flip reverseLookup boolTable <$> letter
    char blockSep
    toMove <- lift =<< flip reverseLookup colorTable <$> letter
    return Position { pieces = pieces, kingMoves = kingMoves, toMove = toMove, pendingKnight = knight }

showKingMoves :: [(Color, KingMoves)] -> String
showKingMoves moves = ifEmpty [noSpecialMoves] $ concatMap showCol [White, Black] where
  showCol col = mapMaybe (convert col) [(boomerangKey, boomerang), (knightKey, knight)] where
  convert col (key, move) = if move (lookupJust col moves) then lookup (key, col) kingMoveTable else Nothing
  ifEmpty a b = if null b then a else b

readKingMoves :: String -> Maybe [(Color, KingMoves)]
readKingMoves str 
  | null str = Nothing
  | str == [noSpecialMoves] = Just [mkMoves White False False, mkMoves Black False False]
  | otherwise = do
    entries <- mapM (flip reverseLookup kingMoveTable) str
    return $ map (mkMoves `ap` has entries knightKey `ap` has entries boomerangKey) [White, Black]
    where
        has entries some col = any (== (some, col)) entries
        mkMoves col k b = (col, KingMoves { knight = k, boomerang = b })

knightKey = "knight"
boomerangKey = "boomerang"

kingMoveTable = [
    ((knightKey, White), 'K'), ((knightKey, Black), 'k'),
    ((boomerangKey, White), 'B'), ((boomerangKey, Black), 'b')
  ]

colorTable = [(White, 'w'), (Black, 'b')]

boolTable = [(True, 'y'), (False, 'n')]

blockSep = ';'
pieceSep = ','
noSpecialMoves = '-'

---- Helpers
lookupJust :: Eq a => a -> [(a, b)] -> b
lookupJust a = fromJust . lookup a

reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup a = lookup a . map swap