module Björn.IO where

-- Björn.IO provides in- and output of basic data types like square and piece.

import Björn.Pieces
import Control.Monad ((<=<))
import Control.Monad.Trans (lift)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (swap)
import Text.Parsec (ParsecT, runParserT, letter, digit)

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

---- PieceKind IO ("K")
showPieceKind :: (Color, PieceKind) -> String
showPieceKind = pure . flip lookupJust pieceKindTable

parsePieceKind :: Parser (Color, PieceKind)
parsePieceKind = lift =<< flip reverseLookup pieceKindTable <$> letter

pieceKindTable :: [((Color, PieceKind), Char)]
pieceKindTable = [
    ((White, Pawn True), 'P'),  ((Black, Pawn True), 'p'),
    ((White, Pawn False), 'Q'), ((Black, Pawn False), 'q'),
    ((White, King), 'K'),       ((Black, King), 'k'),
    ((White, Björn), 'Ö'),      ((Black, Björn), 'ö')
  ]

---- Piece IO ("pb3")
showPiece :: Piece -> String
showPiece pc = showPieceKind (color pc, kind pc) ++ showSquare (square pc)

parsePiece :: Parser Piece
parsePiece = do
    (color, kind) <- parsePieceKind
    square <- parseSquare
    return $ Piece { color = color, kind = kind, square = square }

---- Helpers
lookupJust :: Eq a => a -> [(a, b)] -> b
lookupJust a = fromJust . lookup a

reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup a = lookup a . map swap