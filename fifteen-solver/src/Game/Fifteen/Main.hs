module Game.Fifteen.Main where

import Data.List
import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.TestGen
import Game.Fifteen.Types
import System.Environment
import System.Exit

demo :: Board -> IO ()
demo bd = do
  let steps : _ = solveBoard bd
  Just bd' <- pprSteps bd steps
  pprBoard bd'

raw3 :: String
raw3 =
  unlines
    [ "8 6 7"
    , "2 5 4"
    , "3 _ 1"
    ]

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
    ["stdin"] -> do
      xs <- getContents
      let Just bd = mkBoardFromRaw xs
          steps : _ = solveBoard bd
      putStrLn $ intercalate "|" $ fmap (\(x, y) -> show x <> "," <> show y) steps
    ["dev"] -> do
      let Just bd =
            mkBoardFromRaw raw5
          steps : _ = solveBoard bd
      print steps
    "testgen" : _ -> testGen
    _ -> do
      putStrLn "<prog> dev: for manual debugging"
      putStrLn "<prog> stdin: read inputs from stdin and return solutions in an easily parsable format."
      putStrLn "<prog> testgen: generate test bundle or solve an eixsting bundle."
      exitFailure
