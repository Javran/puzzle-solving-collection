{-# LANGUAGE ScopedTypeVariables #-}

module Game.Fifteen.SolvabilitySpec where

import Data.List
import qualified Data.Vector as V
import Game.Fifteen.Solvability
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "mergeSortFromListN" $
  prop "compare with library sort" $
    \(xs :: [Int]) ->
      let n = length xs
          (ys, _) = mergeSortFromListN n xs
       in V.toList ys == sort xs
