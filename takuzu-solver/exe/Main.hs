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

{-# LANGUAGE
    RecordWildCards
  #-}
module Main (main) where

import Control.Monad
import System.Console.Terminfo

import qualified Data.Set as S
import qualified Data.Vector as V

import Game.Takuzu.Solver
import Game.Takuzu.Parser

{-
  TODO: those examples should be moved to tests.
 -}
exampleRaw0 :: [] ([] Char)
exampleRaw0 =
  [ "    rr  br  "
  , "      r  b b"
  , "  br    r  b"
  , " r r        "
  , "b     r b b "
  , "  b b     b "
  , " r  br  r   "
  , "r    r      "
  , "r   r   bb  "
  , "  r     b   "
  , "      r   rb"
  , "  r  r      "
  ]

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
