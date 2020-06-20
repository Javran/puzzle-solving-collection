{-# LANGUAGE QuasiQuotes #-}

module Game.Minesweeper.ParserSpec where

import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Game.Minesweeper.Parser
import Game.Minesweeper.Types
import Test.Hspec
import Text.ParserCombinators.ReadP
import Text.RawString.QQ

parseAllBoards :: ReadP [BoardRep]
parseAllBoards = many1 (sepP *> fullBoardP)
  where
    sepP = string "====\n"

-- the number of "===="s should exactly match
-- number of test cases.
examples :: String
examples =
  [r|====
7 7
???????
??1122?
??1  1?
?21 13?
?1  1??
?1122??
???????
====
3 4
????
*13?
__2?
====
2 8
?4???5?*
??? ???_
====
1 12
12345678_ *?
====
3 4
E___
1*__
EEE_
|]

spec :: Spec
spec =
  describe "parseBoard" $ do
    specify "examples" $ do
      let expectedLen =
            getSum
              $ foldMap
                (\xs -> if xs == "====" then 1 else 0)
              $ lines examples
      [(xs, [])] <-
        pure $
          readP_to_S (parseAllBoards <* eof) examples
      length xs `shouldBe` expectedLen
    specify "board with missing tiles" $ do
      let result = parseBoard $ unlines ["3 4", "EE__", "1*_E", "__EE"]
      result `shouldSatisfy` isJust
      S.size (brMissing (fromJust result)) `shouldBe` 5
