module Game.Takuzu.Parser where

import Control.Monad
import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP hiding (many)

import Game.Takuzu.Solver

firstLine :: ReadP ((Char,Char), Int)
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
    validCell c = c `elem` [' ',cB,cR]
    tr ' ' = Nothing
    tr c = if c == cB then Just cBlue else Just cRed

fullBoard :: ReadP (Int, [[Maybe Cell]])
fullBoard = do
  (colors, sz) <- firstLine
  lns <- replicateM sz $ boardRow colors
  pure (sz, lns)
