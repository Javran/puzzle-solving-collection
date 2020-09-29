{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Main
  ( main
  )
where

import Data.Maybe
import Game.Fifteen.Common
import qualified Game.Fifteen.ThreeByThree as TBT

main :: IO ()
main = do
  pprBoard demo0
  let smallBoard = fromJust . TBT.fromBoard $ demo0
      goal = fromJust . TBT.fromBoard $ goalBoard 3
  print $ head (TBT.solveBoard goal smallBoard)
