module Game.Kuromasu.Parser where

import Text.ParserCombinators.ReadP

{-
  TODO: input syntax.

  - first line: <rows> <cols> (two numbers)
  - the next <rows> lines are all space separated.
    + a number indicates a blue cell with that number on it
    + 'b' means a blue cell (without number)
    + 'r' mean a red cell.

  - last line consists of an arbitrary number of '=' (or can simply leave this line empty).
  - The input file can contain multiple records, all following the same syntax.

 -}
