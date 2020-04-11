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
module Game.Takuzu.Main
  ( main
  ) where

import Control.Monad
import System.Console.Terminfo

import qualified Data.Set as S
import qualified Data.Vector as V

import Game.Takuzu.Solver

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
  [ "  b b b     "
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

example :: [[Maybe Cell]]
example = (fmap . fmap) tr exampleRaw
  where
    tr ' ' = Nothing
    tr 'r' = Just cRed
    tr 'b' = Just cBlue
    tr _ = undefined


pprBoard :: Terminal -> Board V.Vector -> IO ()
pprBoard term Board{..} = do
  putStrLn $ "Side length: " <> show bdLen
  putStrLn $ "Pending cells: " <> show (S.size bdTodos)
  putStrLn $ "Row candidate counts: " <> show (V.map S.size bdRowCandidates)
  putStrLn $ "Col candidate counts: " <> show (V.map S.size bdColCandidates)
  putStrLn "++++ Board Begin ++++"
  forM_ [0..bdLen-1] $ \r -> do
    forM_ [0..bdLen-1] $ \c -> do
      let cell = bdCells V.! bdToFlatInd (r,c)
      case cell of
        Nothing -> putStr " "
        Just b ->
          case getCapability term withForegroundColor of
            Nothing -> putStr $ if b then "1" else "0"
            Just useColor ->
              let color = if b then Red else Blue
                  rendered = termText "█"
              in runTermOutput term (useColor color rendered)
    putStrLn ""
  putStrLn "---- Board End ----"


main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just bd = mkBoard 6 example
  pprBoard term bd
  pprBoard term $ trySolve bd
