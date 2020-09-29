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
  let smallBoard = fromJust . TBT.fromBoard $ demo0
      goal = fromJust . TBT.fromBoard $ goalBoard 3
      steps:_ = (TBT.solveBoard goal smallBoard)
  pprSteps demo0 steps
