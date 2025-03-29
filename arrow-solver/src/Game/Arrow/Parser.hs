module Game.Arrow.Parser where

import Control.Monad
import Data.Char
import Game.Arrow.Types
import Text.ParserCombinators.ReadP

{-

  raw input format design:

  - first line: an integer representing modulo
  - seccond line: "square" | "hexagon" followed by side length
  - following lines are all space-separated integers

 -}

newlineP :: ReadP ()
newlineP = void (char '\n')

intP :: ReadP Int
intP = read <$> munch1 isDigit

puzzleTypeP :: ReadP PuzzleType
puzzleTypeP = do
  ps <- (Square <$ string "square") <++ (Hexagon <$ string "hexagon")
  _ <- char ' '
  side <- intP <* newlineP
  pure (ps, side)

gridP :: PuzzleType -> ReadP [[Int]]
gridP = \case
  (Square, sz) ->
    replicateM sz (intsOfLen sz)
  (Hexagon, sz) ->
    let lens = [sz .. sz * 2 - 1] <> [sz * 2 - 2, sz * 2 - 3 .. sz]
     in mapM intsOfLen lens
  where
    intsOfLen l = do
      xs <- intP `sepBy` munch1 (== ' ')
      guard $ length xs == l
      munch (== ' ') *> newlineP
      pure xs

puzzleP :: ReadP Puzzle
puzzleP = do
  opMod <- intP <* newlineP
  pzType <- puzzleTypeP
  grid <- gridP pzType
  pure $ Puzzle {opMod, pzType, grid}

fromRawString :: String -> Maybe Puzzle
fromRawString raw = case readP_to_S (puzzleP <* skipSpaces <* eof) raw of
  [(v, "")] -> Just v
  _ -> Nothing
