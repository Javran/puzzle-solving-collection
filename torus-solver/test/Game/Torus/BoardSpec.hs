{-# LANGUAGE ScopedTypeVariables #-}

module Game.Torus.BoardSpec where

import qualified Data.List.Split
import Game.Torus.Board
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype SmallBoard
  = SmallBoard Board
  deriving (Show)

mkArbitraryBoard :: (Int, Int) -> Gen Board
mkArbitraryBoard range = do
  rows <- choose range
  cols <- choose range
  let xs = [1 .. rows * cols]
  xs' <- shuffle xs
  let Just bd =
        mkBoard
          ( (rows, cols)
          , Data.List.Split.chunksOf cols xs'
          )
  pure bd

instance Arbitrary SmallBoard where
  arbitrary = SmallBoard <$> mkArbitraryBoard (3, 8)

-- convert a triple to a random move restricted to Board range. useful for testing.
convertToMove :: Board -> (NonNegative Int, Int, Int) -> Move
convertToMove Board {bdDims = (rows, cols)} (NonNegative ty, ind, st) =
  cons (ind `mod` restrict) st
  where
    (cons, restrict) = case ty `mod` 4 of
      0 -> (MoveLeft, rows)
      1 -> (MoveRight, rows)
      2 -> (MoveUp, cols)
      3 -> (MoveDown, cols)
      _ -> error "unreachable"

spec :: Spec
spec = do
  describe "rotateLeft" $ do
    specify "examples" $
      rotateLeft 3 "abcde" `shouldBe` "deabc"
    prop "associativity" $
      \(Positive a) (Positive b) (NonEmpty (xs :: [Int])) ->
        rotateLeft b (rotateLeft a xs) === rotateLeft (a + b) xs
    prop "periodicity" $
      \(Positive a) (NonEmpty (xs :: [Int])) ->
        let len = length xs
         in rotateLeft a xs === rotateLeft (a + len) xs

  describe "simplifyMoves" $ do
    prop "result-preserving & non-increasing" $
      \(SmallBoard bd) xs ->
        let moves = fmap (convertToMove bd) xs
            moves' = simplifyMoves bd moves
         in label "result-preserving" (applyMoves bd moves === applyMoves bd moves')
              .&&. label "non-increasing" (length moves >= length moves')
