{-# LANGUAGE QuasiQuotes #-}

module Game.Minesweeper.ParserSpec where

import Data.Monoid
import Game.Minesweeper.Parser
import Test.Hspec
import Text.ParserCombinators.ReadP
import Text.RawString.QQ

parseAllBoards :: ReadP [TmpBoard]
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
|]

spec :: Spec
spec =
  describe "parseBoard"
    $ specify "examples"
    $ do
      let expectedLen =
            getSum
              $ foldMap
                (\xs -> if xs == "====" then 1 else 0)
              $ lines examples
      [(xs, [])] <-
        pure $
          readP_to_S (parseAllBoards <* eof) examples
      length xs `shouldBe` expectedLen
