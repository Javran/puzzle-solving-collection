module Game.Torus.Main
  ( main
  )
where

import Game.Torus.Parser

main :: IO ()
main = print (parseBoard demo0)
