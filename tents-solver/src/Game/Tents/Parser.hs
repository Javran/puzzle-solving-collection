{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Tents.Parser
  ( loadPuzzles
  , parseBoard
  , parseBatchBoards
  )
where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Game.Tents.Types
import Paths_tents_solver
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

loadPuzzles :: IO [(String, BoardRep)]
loadPuzzles = do
  fName <- getDataFileName "data/puzzles.txt"
  raw <- readFile fName
  let bds = parseBatchBoards raw
  pure bds

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
  let brRowTentCounts = V.fromListN rows (snd <$> zippedResults)
      joinedLines = concatMap fst zippedResults
      brBoard =
        M.fromList
          . mapMaybe (\(coord, m) -> (coord,) <$> m)
          $ zip [(row, col) | row <- [0 .. rows -1], col <- [0 .. cols -1]] joinedLines
  brColTentCounts <- V.fromListN cols <$> lastLine cols
  pure
    BoardRep
      { brDims
      , brRowTentCounts
      , brColTentCounts
      , brBoard
      }

parseBoard :: String -> Maybe BoardRep
parseBoard raw = case readP_to_S (boardRep <* eof) raw of
  [(v, "")] -> pure v
  _ -> Nothing

puzzleIdLine :: ReadP String
puzzleIdLine =
  string "# " *> munch1 (not . isSpace) <* char '\n'

oneBoardInBatch :: ReadP (String, BoardRep)
oneBoardInBatch = (,) <$> puzzleIdLine <*> boardRep

parseBatchBoards :: String -> [(String, BoardRep)]
parseBatchBoards raw = case readP_to_S (many oneBoardInBatch <* eof) raw of
  [(v, "")] -> v
  _ -> []
