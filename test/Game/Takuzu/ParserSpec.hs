module Game.Takuzu.ParserSpec where

import Test.Hspec

import Game.Takuzu.Parser
import Paths_takuzu_solver

spec :: Spec
spec =
  describe "parseBoards" $
    specify "puzzles file" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      let expectedCount =
            length $ filter (\xs -> not (null xs) && all (== '=') xs) $ lines content
          parsed = parseBoards content
      expectedCount `shouldSatisfy` (> 0)
      length parsed `shouldBe` expectedCount
