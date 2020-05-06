{-
  Solver for https://en.wikipedia.org/wiki/Takuzu

  Credit to game "0h h1" by Q42 for introducing me to this game.
  If you see a lot of mention of color blue and red instead
  of 0 and 1, that's why.

  The decision that blue=0 and red=1 is made because 'b' < 'r' alphabetically,
  this allows us to accept any raw table with ' ' and two more other characters
  in it, and assign 0,1 to these two characters in order.
  e.g. "101 " will result in the exact same input representation as "rbr ".
 -}

module Main (main) where

import System.Console.Terminfo

import Game.Takuzu.Parser
import Game.Takuzu.Solver

exampleRaw :: [] ([] Char)
exampleRaw =
  [ "br 12"
  , "  b b b     "
  , "b  r   r   b"
  , "     b     r"
  , " b     r  b "
  , "  bb   rr b "
  , "        r   "
  , " b  r      r"
  , "     b    r "
  , "bb  rb  b   "
  , "   r     r  "
  , "r r    rb  b"
  , "r   b     r "
  ]

example :: (Int, [[Maybe Cell]])
example = v
  where
    Just v = parseBoard (unlines exampleRaw)

main :: IO ()
main = do
  term <- setupTermFromEnv
  let (sz, bdRaw) = example
      Just bd = mkBoard (sz `quot` 2) bdRaw
  pprBoard term bd
  pprBoard term $ trySolve bd
