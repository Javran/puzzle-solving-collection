module Main
  ( main
  )
where

import Parser

main :: IO ()
main = print (parseBoard rawPuzzle0)
