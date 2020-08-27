module Game.Tents.SolverSpec where

import Game.Tents.Solver
import Game.Tents.Types
import Test.Hspec

spec :: Spec
spec =
  describe "fillLine" $ do
    specify "already completed" $ do
      fillLine 1 [Just Empty, Just Tree, Just Tent]
        `shouldBe` [[Empty, Tree, Tent]]
      fillLine 0 [Just Empty, Just Empty, Just Tree, Just Empty]
        `shouldBe` [[Empty, Empty, Tree, Empty]]
    specify "already completed, unsat" $ do
      fillLine 1 [Just Tent, Just Tent, Just Empty]
        `shouldBe` []
    specify "no constraint" $ do
      fillLine 1 [Just Empty, Nothing, Nothing]
        `shouldBe` [ [Empty, Empty, Tent]
                   , [Empty, Tent, Empty]
                   ]
    specify "must alter" $ do
      fillLine 2 (replicate 3 Nothing)
        `shouldBe` [[Tent, Empty, Tent]]
      fillLine 3 (replicate 5 Nothing)
        `shouldBe` [[Tent, Empty, Tent, Empty, Tent]]
    specify "some degrees of freedom" $ do
      fillLine 2 (replicate 5 Nothing)
        `shouldBe` [ [Empty, Empty, Tent, Empty, Tent]
                   , [Empty, Tent, Empty, Empty, Tent]
                   , [Empty, Tent, Empty, Tent, Empty]
                   , [Tent, Empty, Empty, Empty, Tent]
                   , [Tent, Empty, Empty, Tent, Empty]
                   , [Tent, Empty, Tent, Empty, Empty]
                   ]
      fillLine 3 [Just Tent, Nothing, Nothing, Nothing, Just Tree, Just Tent, Nothing, Nothing, Nothing]
        `shouldBe` [ [Tent, Empty, Empty, Empty, Tree, Tent, Empty, Empty, Tent]
                   , [Tent, Empty, Empty, Empty, Tree, Tent, Empty, Tent, Empty]
                   , [Tent, Empty, Empty, Tent, Tree, Tent, Empty, Empty, Empty]
                   , [Tent, Empty, Tent, Empty, Tree, Tent, Empty, Empty, Empty]
                   ]
    specify "no extra tent needed" $ do
      fillLine 0 (replicate 10 Nothing)
        `shouldBe` [replicate 10 Empty]
      fillLine 2 [Just Tent, Nothing, Nothing, Nothing, Just Tree, Just Tent, Nothing, Nothing, Nothing]
        `shouldBe` [[Tent, Empty, Empty, Empty, Tree, Tent, Empty, Empty, Empty]]
