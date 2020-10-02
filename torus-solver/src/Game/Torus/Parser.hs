module Game.Torus.Parser where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

demo0 :: String
demo0 =
  unlines
    [ "6 6"
    , "25 22 19 31 27 3"
    , "15 32 9 33 16 14"
    , "8 29 10 28 11 18"
    , "23 6 34 21 1 26"
    , "24 5 35 4 2 36"
    , "20 17 7 13 30 12"
    ]

type BoardRep = ((Int, Int), [[Int]])

intLine :: ReadP [Int]
intLine =
  (fmap . fmap) read (munch1 isDigit `sepBy1` char ' ') <* char '\n'

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
