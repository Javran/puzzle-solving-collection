{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Main where

import Data.Maybe
import Game.Fifteen.Common
import Game.Fifteen.Human
import qualified Game.Fifteen.ThreeByThree as TBT

mainSmall :: IO ()
mainSmall = do
  let smallBoard = fromJust . TBT.fromBoard $ demo0
      goal = fromJust . TBT.fromBoard $ goalBoard 3
      steps : _ = (TBT.solveBoard goal smallBoard)
  _ <- pprSteps demo0 steps
  pure ()

main :: IO ()
main = do
  let goal = goalBoard 3
      steps:_ = solveBoard goal demo0
  Just bd' <- pprSteps demo0 steps
  pprBoard (fromJust $ subBoard bd')
