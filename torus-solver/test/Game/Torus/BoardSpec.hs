{-# LANGUAGE ScopedTypeVariables #-}

module Game.Torus.BoardSpec where

import Game.Torus.Board
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
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
