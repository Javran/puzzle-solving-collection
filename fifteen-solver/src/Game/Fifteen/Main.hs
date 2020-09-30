{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Main where

import Game.Fifteen.Common
import Game.Fifteen.Human
import Game.Fifteen.Types
import System.Environment
import Data.List

demo :: Board -> IO ()
demo bd = do
  let goal = goalBoard (bdSize bd)
      steps : _ = solveBoard goal bd
  Just bd' <- pprSteps bd steps
  pprBoard bd'

raw5 :: String
raw5 =
  unlines
    [ "16 20 23 13 11"
    , "15 6 9 19 4"
    , "_ 2 1 10 17"
    , "22 24 21 7 5"
    , "12 18 3 8 14"
    ]

raw8 :: String
raw8 =
  unlines
    [ "30 54 59 7 26 34 40 5"
    , "35 16 25 44 29 62 32 58"
    , "38 17 6 4 51 43 10 13"
    , "18 21 31 45 9 37 42 1"
    , "50 14 _ 41 11 36 22 53"
    , "47 27 55 52 24 8 19 33"
    , "63 46 28 15 12 61 49 39"
    , "3 2 56 48 57 20 23 60"
    ]

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] -> do
      let raw =
            unlines
              [ "8 6 7"
              , "2 5 4"
              , "3 _ 1"
              ]
          Just bd = mkBoardFromRaw raw
      demo bd
    ["stdin"] -> do
      xs <- getContents
      let Just bd = mkBoardFromRaw xs
          goal = goalBoard (bdSize bd)
          steps : _ = solveBoard goal bd
      putStrLn $ intercalate "|" $ fmap (\(x,y) -> show x <> ","<> show y) steps

{-
  TODO:
2 7 17 9 16
8 14 1 22 6
21 18 20 3 10
13 24 12 _ 15
4 19 23 11 5

9 1 19 16 3
7 13 4 10 11
14 21 _ 22 8
18 2 17 23 15
6 5 24 12 20

1 2 3 4 5
6 7 8 9 10
11 13 14 17 20
16 15 18 12 19
21 22 23 24 _

 -}
