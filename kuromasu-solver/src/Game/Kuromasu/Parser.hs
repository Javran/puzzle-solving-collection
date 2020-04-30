module Game.Kuromasu.Parser
  ( parseBoard
  ) where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

import Game.Kuromasu.Solver

{-
  TODO: input syntax.

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
row cols = do
  cs <- cell `sepBy` char ' '
  _ <- char '\n'
  guard $ length cs == cols
  pure cs

endLine :: ReadP ()
endLine = void $ munch (== '=') *> char '\n'

type BoardRep = ((Int, Int), [[CellRep]])

board :: ReadP BoardRep
board = do
  dims@(rows, cols) <- firstLine
  bd <- replicateM rows (row cols)
  endLine
  pure (dims, bd)

parseBoard :: String -> Maybe [BoardRep]
parseBoard raw = case readP_to_S (many board <* eof) raw of
  [(v, "")] -> Just v
  _ -> Nothing
