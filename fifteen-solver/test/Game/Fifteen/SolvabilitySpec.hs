{-# LANGUAGE ScopedTypeVariables #-}

module Game.Fifteen.SolvabilitySpec where

import Control.Monad
import Data.List
import qualified Data.List.Split
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.Solvability
import Game.Fifteen.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

genBoard :: (Int, Int) -> Gen Board
genBoard range = do
  sz <- choose range
  let lastNum = sz * sz -1
      tr x = if x == lastNum then Nothing else Just x
  xs <- shuffle [0 .. lastNum]
  pure $ mkBoard $ (fmap . fmap) tr $ Data.List.Split.chunksOf sz xs

newtype SmallBoard = SmallBoard Board deriving (Show)

instance Arbitrary SmallBoard where
  arbitrary = SmallBoard <$> genBoard (3, 6)

spec :: Spec
spec = do
  describe "mergeSortFromListN" $ do
    specify "small example" $ do
      {-
        check against example in
        https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
       -}
      let v = [12 :: Int, 1, 10, 2, 7, 11, 4, 14, 5, 9, 15, 8, 13, 6, 3]
          (_, count) = mergeSortFromListN (length v) v
      count `shouldBe` 49
    prop "compare with library sort" $
      \(xs :: [Int]) ->
        let n = length xs
            (ys, _) = mergeSortFromListN n xs
         in V.toList ys == sort xs

  describe "isSolvable" $ do
    let verifyBoardFromRaw rawSource expectSolvable = do
          let bd = mkBoard rawSource
              goal = goalBoard (bdSize bd)
          isSolvable bd `shouldBe` expectSolvable
          {-
            Note that an unsolvable board will take tooo loooong to finish its search,
            so instead of doing that, we simply verify that if we manipulate an unsolvable
            board so its solvabilty flips to solvable, and we can solve a solvable board
            with our heuristic search fairly quickly.
           -}
          if isSolvable bd
            then solveBoard goal bd `shouldSatisfy` not . null
            else solveBoard goal (flipSolvability bd) `shouldSatisfy` not . null

    specify "examples (odd size, solvable)" $ do
      verifyBoardFromRaw
        [ [Just 6, Just 1, Just 0]
        , [Just 5, Just 7, Just 3]
        , [Just 2, Nothing, Just 4]
        ]
        True
    specify "examples (odd size, unsolvable)" $
      verifyBoardFromRaw
        [ [Just 1, Just 6, Just 0]
        , [Just 5, Just 7, Just 3]
        , [Just 2, Nothing, Just 4]
        ]
        False
    specify "examples (even size, solvable)" $
      verifyBoardFromRaw
        [ [Just 11, Just 12, Just 14, Just 4]
        , [Just 7, Just 8, Just 2, Just 9]
        , [Just 1, Just 3, Just 13, Nothing]
        , [Just 6, Just 5, Just 10, Just 0]
        ]
        True
    specify "examples (even size, unsolvable)" $
      verifyBoardFromRaw
        [ [Just 11, Just 12, Just 14, Just 4]
        , [Just 7, Just 8, Just 2, Just 9]
        , [Just 1, Just 3, Just 13, Just 6]
        , [Nothing, Just 5, Just 10, Just 0]
        ]
        False
    prop "SmallBoard" $
      \(SmallBoard bd) ->
        let goal = goalBoard (bdSize bd)
         in if isSolvable bd
              then
                label "solvable" $
                  not . null $ solveBoard goal bd
              else
                label "not solvable" $
                  not . null $ solveBoard goal (flipSolvability bd)
