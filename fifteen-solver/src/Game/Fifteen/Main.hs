{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Main
  ( main
  )
where

import Control.Monad
import qualified Data.Map as M
import Game.Fifteen.Types
import Game.Fifteen.Common
import qualified Game.Fifteen.ThreeByThree as TBT

main :: IO ()
main = do
  print (TBT.solveBoard (goalBoard 3) demo0)
  -- demo1 solution in reverse:
  -- [[(2,2),(1,2),(1,1),(2,1),(2,0),(0,0),(0,1),(2,1),(2,0),(1,0),(1,2),(2,2),(2,0),(1,0),(1,2),(0,2),(0,0)]]

{-
real	343.50s
user	4026.25s
sys	2756.45s
 -}
