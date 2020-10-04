module Game.Torus.Parser
  ( BoardRep
  , pBoard
  , parseBoard
  )
where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

type BoardRep = ((Int, Int), [[Int]])

{-
  Input format:

  two numbers on first line:

  <row> <col>

  the followed by space-separated lines representing
  numbers on each tiles.

 -}

-- parse non-empty lines consisting of just integers.
intLine :: ReadP [Int]
intLine =
  (fmap . fmap) read (munch1 isDigit `sepBy1` char ' ')
    <* char '\n'

rowAndCol :: ReadP (Int, Int)
rowAndCol = do
  [row, col] <- intLine
  pure (row, col)

pBoard :: ReadP BoardRep
pBoard = do
  dims@(rowN, colN) <- rowAndCol
  bd <- replicateM rowN $ do
    xs <- intLine
    guard $ length xs == colN
    pure xs
  pure (dims, bd)

parseBoard :: String -> Maybe BoardRep
parseBoard raw = case readP_to_S pBoard raw of
  [(v, "")] -> pure v
  _ -> Nothing
