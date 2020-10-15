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
      {-
        The main purpose of this section is to show whether
        a change in solving tactic has a positive overall effect.
        This is done by running the solver against existing bundle of puzzles,
        and then have their results compared.
       -}
      bundle <-
        runIO $
          getDataFileName "data/puzzle-bundle.txt"
            >>= loadPuzzleBundle
      bundleMoves <-
        runIO $
          getDataFileName "data/puzzle-bundle-moves.txt"
            >>= loadPuzzleBundleMoves

      for_ (M.toList bundle) $ \(boardId, bd) -> do
        describe boardId $ do
          let moves : _ = solveBoard bd
              moveCount = length moves
              desc = case bundleMoves M.!? boardId of
                Nothing -> show moveCount <> " (?)"
                Just oldMoveCount ->
                  case compare moveCount oldMoveCount of
                    EQ -> show moveCount <> " (=)"
                    LT -> show oldMoveCount <> " -> " <> show moveCount <> " (less)"
                    GT -> show oldMoveCount <> " -> " <> show moveCount <> " (more)"
          specify ("moves: " <> desc) $
            () `shouldBe` ()
