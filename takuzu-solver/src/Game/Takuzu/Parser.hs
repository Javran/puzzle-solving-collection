module Game.Takuzu.Parser
  ( parseBoard
  , parseBoards
  ) where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

import Game.Takuzu.Solver

{-
  Puzzle input file syntax:

  - first line: <c0><c1> <num>, in which c0 and c1 are single character
    for the different colors we are working with, and the board is of size num x num.

  - for the following <num> lines, ' ' indicates an empty cell, and <c1> and <c2>
    indicate two different cells.

  - last line consists of an arbitrary number of '=' (or can simply leave this line empty).

  - The input file can contain multiple records, all following the same syntax.

 -}

firstLine :: ReadP ((Char, Char), Int)
firstLine = do
  {-
    begin with two colors that are considered blue and red
   -}
  cB <- get
  cR <- get
  _ <- char ' '
  -- followed by size of the board, which must be an even number.
  rawDigits <- munch1 isDigit
  [(sz, "")] <- pure $ reads rawDigits
  guard $ even sz
  _ <- char '\n'
  pure ((cB, cR), sz)

boardRow :: (Char, Char) -> ReadP [Maybe Cell]
boardRow (cB, cR) = do
  raw <- munch validCell <* char '\n'
  pure $ tr <$> raw
  where
    validCell c = c `elem` [' ', cB, cR]
    tr ' ' = Nothing
    tr c = if c == cB then Just cBlue else Just cRed

fullBoard :: ReadP (Int, [[Maybe Cell]])
fullBoard = do
  (colors, sz) <- firstLine
  lns <- replicateM sz $ boardRow colors
  pure (sz, lns)

inputSep :: ReadP ()
inputSep = () <$ (munch (== '=') >> char '\n')

parseBoard :: String -> Maybe (Int, [[Maybe Cell]])
parseBoard raw = case readP_to_S (fullBoard <* eof) raw of
  [(v, "")] -> Just v
  _ -> Nothing

parseBoards :: String -> [(Int, [[Maybe Cell]])]
parseBoards raw = case readP_to_S (many1 (fullBoard <* inputSep) <* eof) raw of
  [(v, "")] -> v
  _ -> []
