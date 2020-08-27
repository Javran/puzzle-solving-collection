{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Tents.Parser where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP
import Game.Tents.Types

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

boardRep :: ReadP BoardRep
boardRep = do
  brDims@(rows, cols) <- dimsLine
  zippedResults <- replicateM rows (boardLine cols)
  let brRowTreeCounts = V.fromListN rows (snd <$> zippedResults)
      joinedLines = concatMap fst zippedResults
      brBoard =
        M.fromList
          . mapMaybe (\(coord, m) -> (coord,) <$> m)
          $ zip [(row, col) | row <- [0 .. rows -1], col <- [0 .. cols -1]] joinedLines
  brColTreeCounts <- V.fromListN cols <$> lastLine cols
  pure
    BoardRep
      { brDims
      , brRowTreeCounts
      , brColTreeCounts
      , brBoard
      }

parseBoard :: String -> Maybe BoardRep
parseBoard raw = case readP_to_S (boardRep <* eof) raw of
  [(v, "")] -> pure v
  _ -> Nothing
