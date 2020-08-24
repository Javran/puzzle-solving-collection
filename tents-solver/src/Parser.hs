module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

{-
  syntax draft:
  - first line: <rows> <cols>
  - next <rows> lines: board presentation <space> <number>
    + board:
      - '?': unknown
      - '_': empty
      - 'R': tree (tRee)
      - 'E': tent (tEnt)
  - last line: numbers, space-separated.

 -}
rawPuzzle0 :: String
rawPuzzle0 =
  unlines
    [ "9 8"
    , "??R????_ 1"
    , "???R?R?? 2"
    , "R??????? 1"
    , "???????? 1"
    , "?R??R?RR 2"
    , "??R???R? 2"
    , "?R?????R 2"
    , "?????R?? 2"
    , "ER?????? 1"
    , "3 2 1 1 2 1 2 2"
    ]

data Cell = Empty | Tree | Tent

type Coord = (Int, Int) -- (row, col)

data BoardRep = BoardRep
  { brDim :: (Int, Int) -- (rows, cols)
  , brRowTreeCounts :: V.Vector Int -- # of trees in each row, must be of length rows
  , brColTreeCounts :: V.Vector Int -- same but for cols
  , brBoard :: M.Map Coord Cell
  }

int :: ReadP Int
int = read <$> munch1 isDigit

dimsLine :: ReadP (Int, Int)
dimsLine = (,) <$> (int <* char ' ') <*> (int <* char '\n')

cell :: ReadP (Maybe Cell)
cell =
  (Nothing <$ char '?')
    <++ (Just Empty <$ char '_')
    <++ (Just Tree <$ char 'R')
    <++ (Just Tent <$ char 'E')

boardLine :: Int -> ReadP ([Maybe Cell], Int)
boardLine cols =
  (,)
    <$> (replicateM cols cell <* char ' ')
    <*> (int <* char '\n')

lastLine :: Int -> ReadP [Int]
lastLine cols = do
  xs <- int `sepBy` char ' '
  guard $ length xs == cols
  xs <$ char '\n'
