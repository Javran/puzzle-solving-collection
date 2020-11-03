module Game.Tents.ParserSpec where

import Data.List
import Data.Monoid
import Game.Tents.Parser
import Paths_tents_solver
import Test.Hspec

spec :: Spec
spec =
  describe "parseBatchBoards" $ do
    raw <- runIO $ do
      fName <- getDataFileName "data/puzzles.txt"
      content <- readFile fName
      length content `seq` pure content
    specify "data examples" $ do
      let expectedCount :: Int
          expectedCount =
            getSum
              . foldMap (\l -> if "# " `isPrefixOf` l then 1 else 0)
              $ lines raw
          actual = parseBatchBoards raw
      expectedCount `shouldNotSatisfy` (== 0)
      length actual `shouldBe` expectedCount
