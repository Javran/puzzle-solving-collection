module Game.Takuzu.Parser where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

firstLine :: ReadP ((Char,Char), Int)
firstLine = do
  {-
    begin with two colors that are considered blue and red
   -}
  c0 <- get
  c1 <- get
  _ <- char ' '
  -- followed by size of the board, which must be an even number.
  rawDigits <- munch1 isDigit
  [(sz, "")] <- pure $ reads rawDigits
  guard $ even sz
  _ <- char '\n'
  pure ((c0, c1), sz)
