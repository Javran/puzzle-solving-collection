{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Main
  ( main
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Game.Fifteen.Common
import qualified Game.Fifteen.ThreeByThree as TBT
import Game.Fifteen.Types

main :: IO ()
main = do
  -- print (TBT.solveBoard (goalBoard 3) demo0)
  pprBoard demo0
  let smallBoard = fromJust . TBT.fromBoard $ demo0
  forM_ [(0, 0), (1, 1), (2, 0)] $ \coord -> do
    print coord
    pprBoard (TBT.toBoard $ TBT.swapHole smallBoard coord)

-- demo1 solution in reverse:
-- [[(2,2),(1,2),(1,1),(2,1),(2,0),(0,0),(0,1),(2,1),(2,0),(1,0),(1,2),(2,2),(2,0),(1,0),(1,2),(0,2),(0,0)]]

{-
real	343.50s
user	4026.25s
sys	2756.45s
 -}
