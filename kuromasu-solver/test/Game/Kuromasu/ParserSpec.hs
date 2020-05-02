module Game.Kuromasu.ParserSpec where

import Test.Hspec

import Game.Kuromasu.Parser
import Paths_kuromasu_solver

spec :: Spec
spec =
  describe "parseBoards" $
    specify "puzzles file" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      -- this relies on the fact that we have as many separators ("====")
      -- as valid puzzles in the data file.
      let expectedCount =
            length
            . filter ((&&) <$> (not . null) <*> all (== '='))
            $ lines content
          parsed = parseBoards content
      expectedCount `shouldSatisfy` (> 0)
      length parsed `shouldBe` expectedCount
