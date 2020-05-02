module Game.Kuromasu.SolverSpec where

import Test.Hspec

import Control.Monad.IO.Class
import Game.Kuromasu.Parser
import Game.Kuromasu.Solver
import Paths_kuromasu_solver
import Data.Maybe
import qualified Data.List.Match as LMatch

{-
  TODO: it turns out 0hn0 does not exactly
  follow the game rule of kuromasu. namely:

  - red cells are not allowed to border each other
    horizontally or vertically.

  - the number on that dot include that dot itself.

  For now this issue isn't very important,
  but in future this is definitely worth pointing out explicitly.

  TODO: rules are (according to 0hn0 rather than the original game
  as described in wikipedia page):

  - a number n indicates that cell is blue,
    and it can see n blues in 4 directions (excluding itself)
  - blue dots sees at least one.
    (in other words, blue dots cannot be surrounded by red dots).
 -}


spec :: Spec
spec =
  describe "mkBoardFromRep" $
    specify "puzzles to Board" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      let boardReps = parseBoards content
          boards = mapMaybe mkBoardFromRep boardReps
      LMatch.equalLength boardReps boards `shouldBe` True
      -- TODO
