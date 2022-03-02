module Bjorn.Core.IO (
    Parser, tryParse,
    
    showSquare, parseSquare,
    showPiece, parsePiece,
    showPieceSq, parsePieceSq,
    showKingMoves, readKingMoves,
    showPosition, parsePosition,
    showMove, parseMove
) where

-- Bjorn.IO provides in- and output of various game-related data types like square, piece and position.

import Bjorn.Core.ApplyMove
import Bjorn.Core.MoveGen
import Bjorn.Core.Pieces
import Bjorn.Core.Position
import Bjorn.Core.PosRepr
import Bjorn.Core.Utils
import Control.Monad ((<=<), ap, when)
import Control.Monad.Trans (lift)
import Data.Char (chr, ord, isLetter, isDigit)
import Data.List (delete, intercalate)
import Data.Maybe
import Data.Tuple (swap)
import Text.Parsec

type Parser a = ParsecT String () Maybe a

-- Parse a string using the given parser. Requires the string to end after the parser succeeded.
tryParse :: Parser a -> String -> Maybe a
tryParse p = either (const Nothing) Just <=< runParserT (reqEof p) () ""

reqEof :: Parser a -> Parser a
reqEof p = do
  res <- p
  eof
  return res

---- Square IO ("a8")
showSquare :: Square -> String
showSquare (x, y) = (chr (ord 'a' - 1 + x)) : show y

parseSquare :: Parser Square
parseSquare = do
  x <- letter
  y <- digit
  lift $ toSquare [x,y]

-- The argument must be of the form [letter, digit].
toSquare :: String -> Maybe Square
toSquare [a,b] = do
  x <- validate $ (ord a - ord 'a' + 1)
  y <- validate $ read [b]
  return (x, y) where
    validate i = if (1 <= i && i <= boardSize) then return i else Nothing

---- Piece IO ("K")
showPiece :: (Color, Piece) -> String
showPiece = pure . flip lookupJust pieceTable

parsePiece :: Parser (Color, Piece)
parsePiece = lift =<< flip reverseLookup pieceTable <$> letter

pieceTable = [
    ((White, Pawn True), 'P'),  ((Black, Pawn True), 'p'),
    ((White, Pawn False), 'Q'), ((Black, Pawn False), 'q'),
    ((White, King), 'K'),       ((Black, King), 'k'),
    ((White, Bjorn), 'B'),      ((Black, Bjorn), 'b')
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

reverseLookup :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookup a = lookup a . map swap


---- Move IO ("KBf2#, +cd3")

-- The move must be valid in the position.
showMove :: PosRepr a => a -> Move -> String
showMove pos move = catMaybes [returnKnight, pc, srcX, srcY, mtype, destX, destY, check, mate] where
    when x b = if b then Just x else Nothing
    returnKnight = (cross `when`) $ pendingKnightCheck pos && knightCheck move
    pc = lookup (piece move) pieceDict
    (x1,_):other = map src $ filter ((==) (dest move) . dest) (genPawnMoves pos) -- 1 or 2 elements if a pawn moves
    srcX = ((head . showSquare . src) move `when`) $ isPawn (piece move) && not (null other) && x1 /= x2 where (x2,_) = head other
    srcY = ((last . showSquare . src) move `when`) $ isPawn (piece move) && not (null other) && x1 == x2 where (x2,_) = head other
    mtype = lookup (moveType move) moveTypeDict
    destX:destY:_ = map return $ showSquare (dest move)
    check = (cross `when`) $ givesPawnCheck pos move || not (pendingKnightCheck pos) && knightCheck move
    mate = (hash `when`) $ moveWins pos move

cross = '+'
hash = '#'
pieceDict = [(King, 'K'), (Bjorn, 'B')]
moveTypeDict = [(Knight, 'S'), (Boomerang, 'B')]

data MoveParseError = Syntax | PieceNotAvailable | SpecialMoveNotAvailable | AmbiguousMove | UnreachableDest | CheckNotPossible | NoMate deriving (Eq, Show)

parseMove :: PosRepr a => a -> String -> Either MoveParseError Move
parseMove pos str = case tryParse parseMoveStencil str of
  Nothing -> Left Syntax
  Just stencil -> do
    let player = whoseTurn pos
    let allMoves = genMoves pos
    -- Can return knight check?
    when (returnKnightCheck' stencil && not (pendingKnightCheck pos)) (Left CheckNotPossible)
    when (returnKnightCheck' stencil && check' stencil) (Left Syntax)
    -- Piece available?
    when (piece' stencil == King && null (king pos player)) (Left PieceNotAvailable)
    when (isPawn (piece' stencil) && not (any (srcOk . fst) (pawns pos player))) (Left PieceNotAvailable)
    -- Special move available?
    when (moveType' stencil == Knight && not (hasKnight pos player)) (Left SpecialMoveNotAvailable)
    when (moveType' stencil == Boomerang && not (hasBoomerang pos player)) (Left SpecialMoveNotAvailable)
    -- Find exact move, modulo knight check
    let moves = filter (matches stencil) allMoves
    case filter (not . knightCheck) moves of
      [] -> Left UnreachableDest
      _:_:[] -> Left AmbiguousMove
      [move] -> do
        -- Consider knight check and pawn check
        when (isPawn (piece' stencil) && check' stencil && not (givesPawnCheck pos move)) (Left CheckNotPossible)
        when (piece' stencil == Bjorn && check' stencil) (Left CheckNotPossible)
        let isKnightCheck = returnKnightCheck' stencil || (piece' stencil == King && check' stencil)
        case filter (((==) isKnightCheck) . knightCheck) moves of
          [move] -> when (mate' stencil && not (moveWins pos move)) (Left NoMate) >> return move
          _ -> Left CheckNotPossible
    where
      srcOk sq = all ((==) (fst sq)) (srcX stencil) && all ((==) (snd sq)) (srcY stencil)
      matches stencil move = pieceEq (piece move) (piece' stencil) && dest move == dest' stencil && srcOk (src move)
      pieceEq a b = isPawn a && isPawn b || a == b

data MoveStencil = MoveStencil {
  returnKnightCheck' :: Bool,
  piece' :: Piece, -- (Pawn _), Bjorn, King
  srcX :: Maybe Int,
  srcY :: Maybe Int,
  dest' :: Square,
  moveType' :: MoveType,
  check' :: Bool,
  mate' :: Bool
} deriving Show

mkStencil knight pc srcX srcY dest mtype check mate = MoveStencil { returnKnightCheck' = knight, piece' = pc, srcX = srcX, srcY = srcY, dest' = dest, moveType' = mtype, check' = check, mate' = mate }

parseMoveStencil :: Parser MoveStencil
parseMoveStencil = do
  returnKnight <- has cross
  pc <- lookupChar (Pawn False) (map swap pieceDict)
  mtype <- lookupChar Normal (map swap moveTypeDict)
  a <- letter <|> digit
  b <- letter <|> digit
  if isLetter a && isDigit b then do
    dest <- lift $ toSquare [a,b]
    check <- has cross
    mate <- has hash
    return $ mkStencil returnKnight pc Nothing Nothing dest mtype check mate
  else do
    when (not (isPawn pc)) (fail "Specify src only for pawn")
    c <- letter <|> digit
    if isLetter b && isDigit c then do
      dest <- lift $ toSquare [b,c]
      let srcX = if isLetter a then fmap fst (toSquare [a,'1']) else Nothing
      let srcY = if isDigit a then fmap snd (toSquare ['a',a]) else Nothing
      check <- has cross
      mate <- has hash
      return $ mkStencil returnKnight pc srcX srcY dest mtype check mate
    else do
      fail "Invalid src/dest declaration"
  where
    has :: Char -> Parser Bool
    has c = option False (fmap (return True) (char c))
    lookupChar :: a -> [(Char, a)] -> Parser a
    lookupChar def dict = option def (fmap (flip lookupJust dict) (oneOf (map fst dict)))