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

demo1 :: String
demo1 =
  unlines
    [ "3 7"
    , "21 20 19 18 17 16 15"
    , "14 13 12 11 10 9 8"
    , "7 6 5 4 3 2 1"
    ]

demo2 :: String
demo2 =
  unlines
    [ "4 6"
    , "24 23 22 21 20 19"
    , "17 18 16 15 14 12"
    , "13 11 10 9 8 7"
    , "5 6 4 2 3 1"
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
