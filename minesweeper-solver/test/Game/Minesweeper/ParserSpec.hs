{-# LANGUAGE QuasiQuotes #-}

module Game.Minesweeper.ParserSpec where

import Data.Maybe
import Game.Minesweeper.Parser
import Test.Hspec
import Text.RawString.QQ

examples :: [String]
examples =
  [ [r|7 7
???????
??1122?
??1  1?
?21 13?
?1  1??
?1122??
???????
|],
    [r|3 4
????
*13?
__2?
|],
    [r|2 8
?4???5?*
??? ???_
|],
    [r|1 12
12345678_ *?
|]
  ]

spec :: Spec
spec =
  describe "parseBoard"
    $ specify "examples"
    $ fmap parseBoard examples `shouldSatisfy` all isJust
