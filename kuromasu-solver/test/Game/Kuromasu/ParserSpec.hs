module Game.Kuromasu.ParserSpec where

import Data.Maybe
import Test.Hspec

import Game.Kuromasu.Parser
import Paths_kuromasu_solver

import qualified Data.List.Match as LMatch

spec :: Spec
spec =
  describe "parseBoards & mkBoardFromRep" $
    specify "puzzles file" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      -- this relies on the fact that we have as many separators ("====")
      -- as valid puzzles in the data file.
      let expectedCount =
            length
            . filter ((&&) <$> (not . null) <*> all (== '='))
            $ lines content
          boardReps = parseBoards content
          boards = mapMaybe mkBoardFromRep boardReps
      expectedCount `shouldSatisfy` (> 0)
      length boardReps `shouldBe` expectedCount
      LMatch.equalLength boardReps boards `shouldBe` True
