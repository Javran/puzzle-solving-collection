{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  ) where

import System.Console.Terminfo

import Game.Kuromasu.Solver
import Game.Kuromasu.Parser

-- TODO: specify rows and cols, and colors
exampleRaw1 :: [String]
exampleRaw1 =
  [ "? ? 3 2 ? ? r 1 1"
  , "r 4 ? r ? ? ? ? ?"
  , "? ? ? ? ? ? 7 ? ?"
  , "? ? ? ? 8 8 ? 9 8"
  , "5 ? ? 6 ? ? ? ? 4"
  , "5 r ? ? ? r 5 ? ?"
  , "? ? 6 ? 3 ? ? ? r"
  , "? r 5 ? r ? 4 r 1"
  , "? ? ? 3 ? 5 ? 3 ?"
  ]

exampleRaw :: [String]
exampleRaw =
  [ "1 ? 3 ? ? ? ? 3 ?"
  , "? ? ? ? 4 4 ? ? r"
  , "? 9 8 ? 8 ? ? ? ?"
  , "2 ? ? ? ? ? ? ? 2"
  , "? ? ? ? 2 ? ? 6 4"
  , "? r ? 8 ? ? 8 ? 7"
  , "2 ? ? ? ? r ? 4 ?"
  , "? ? 5 ? ? ? ? ? 3"
  , "? ? 7 ? ? ? r ? r"
  ]

exampleRaw2 :: [String]
exampleRaw2 =
  [ "? r 2 r ? r ? 6 2"
  , "r 4 ? 6 4 ? 2 ? ?"
  , "r ? ? 7 ? ? r 5 ?"
  , "? ? ? ? ? ? ? ? 7"
  , "r 4 r ? r ? ? 6 6"
  , "4 ? ? r 4 1 r ? ?"
  , "? ? ? 6 ? ? ? ? ?"
  , "? ? ? ? 7 ? 6 6 r"
  , "4 5 ? 7 ? ? ? ? ?"
  ]

main :: IO ()
main = do
  term <- setupTermFromEnv
  print (parseBoard $ unlines $ "9 9" : exampleRaw <> ["===="])
  solveAndShow term exampleRaw2

