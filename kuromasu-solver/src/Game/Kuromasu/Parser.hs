module Game.Kuromasu.Parser
  ( parseBoard
  , parseBoards
  , mkBoardFromRep
  ) where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

import Game.Kuromasu.Solver

{-
  - first line: <rows> <cols> (two numbers)
  - the next <rows> lines are all space separated.
    + a number indicates a blue cell with that number on it
    + 'b' means a blue cell (without number)
    + 'r' means a red cell.
    + '?' means a unknown cell.

  - last line consists of an arbitrary number of '=' (or can simply leave this line empty).
  - The input file can contain multiple records, all following the same syntax.
 -}

firstLine :: ReadP (Int, Int)
firstLine =
  (,)
    <$> (read <$> munch1 isDigit <* char ' ')
    <*> (read <$> munch1 isDigit <* char '\n')

{-
  - Left bool, where b indicates a color
  - Right num, where num indicates a number (blue cell implied)
 -}

type CellRep = Maybe (Either Bool Int)

cell :: ReadP CellRep
cell =
  (Nothing <$ char '?')
    <++ (Just (Left cRed) <$ char 'r')
    <++ (Just (Left cBlue) <$ char 'b')
    <++ (Just . Right <$> (read <$> munch1 isDigit))

row :: Int -> ReadP [CellRep]
row colCount = do
  cs <- cell `sepBy` char ' '
  _ <- char '\n'
  guard $ length cs == colCount
  pure cs

endLine :: ReadP ()
endLine = void $ munch (== '=') *> char '\n'

type BoardRep = ((Int, Int), [[CellRep]])

board :: ReadP BoardRep
board = do
  dims@(rows, cols) <- firstLine
  bd <- replicateM rows (row cols)
  pure (dims, bd)

parseBoard :: String -> Maybe BoardRep
parseBoard raw = case readP_to_S (board <* eof) raw of
  [(vs, "")] -> Just vs
  _ -> Nothing

{-
  It is guaranteed that a successful parsing produces consistent shape of data.
  (i.e. expected rows and cols).
 -}
parseBoards :: String -> [BoardRep]
parseBoards raw = case readP_to_S (many (board <* endLine) <* eof) raw of
  [(vs, "")] -> vs
  _ -> []

mkBoardFromRep :: BoardRep -> Maybe (Board, HintMap)
mkBoardFromRep (dims@(rows, cols), cellReps) =
  (,hints)
    <$> foldM
      (\curBd (coord, color) -> updateCell curBd coord color)
      initBoard
      colors
  where
    initBoard = mkBoard dims hints
    coords = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
    hints :: HintMap
    hints = concatMap (uncurry tr) $ zip coords (concat cellReps)
      where
        tr _ Nothing = []
        tr _ (Just (Left _)) = []
        tr coord (Just (Right n)) = [(coord, n)]
    colors :: ColorMap
    colors = concatMap (uncurry tr) $ zip coords (concat cellReps)
      where
        tr _ Nothing = []
        tr coord (Just m) = case m of
          Left color -> [(coord, color)]
          Right _ -> [(coord, cBlue)]
