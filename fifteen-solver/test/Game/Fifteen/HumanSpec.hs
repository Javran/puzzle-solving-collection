module Game.Fifteen.HumanSpec where

import Data.Foldable
import qualified Data.Map.Strict as M
import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.TestGen
import Game.Fifteen.Types
import Paths_fifteen_solver
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype SolvableBoard = SolvableBoard Board deriving (Show)

instance Arbitrary SolvableBoard where
  arbitrary =
    SolvableBoard
      <$> (choose (3, 8) >>= genSolvableBoardOfSize)

spec :: Spec
spec = do
  describe "solveBoard" $ do
    prop "correctness on generated boards" $
      \(SolvableBoard bd) ->
        let solutions = solveBoard bd
            size = bdSize bd
            tag = "size: " <> show size
         in label tag $
              not (null solutions)
                .&&. let moves = head solutions
                      in case applyMoves bd moves of
                           Nothing -> False
                           Just bdFin ->
                             isSolved bdFin
    describe "examples from data files" $ do
      bundle <-
        runIO $
          getDataFileName "data/puzzle-bundle.txt"
            >>= loadPuzzleBundle
      for_ (M.toList bundle) $ \(boardId, bd) -> do
        specify boardId $ do
          let solutions = solveBoard bd
          solutions `shouldSatisfy` not . null
