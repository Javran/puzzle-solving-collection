{-# LANGUAGE QuasiQuotes #-}

module Game.Minesweeper.SolverSpec where

import Control.Monad
import Data.List
import Data.Monoid
import Game.Minesweeper.Parser
import Game.Minesweeper.Solver
import Test.Hspec
import Text.ParserCombinators.ReadP
import Text.RawString.QQ

rawTests :: String
rawTests =
  -- TODO: we can actually make more progress on Test #0
  [r|= #0
7 7
???????
??1122?
??1__1?
?21_13?
?1__1??
?1122??
???????
----
?___**_
?_1122_
_*1__1_
?21_13*
?1__1**
?1122_?
???????
|]

expectedTestCount :: Int
expectedTestCount =
  getSum
    . foldMap
      ( \xs ->
          if "=" `isPrefixOf` xs then 1 else 0
      )
    $ lines rawTests

tests :: [(String, (TmpBoard, TmpBoard))]
tests = case readP_to_S (many fullTestCaseP <* eof) rawTests of
  [(xs, "")] ->
    let len = length xs
     in if len == expectedTestCount
          then xs
          else
            error $
              "Expecting "
                <> show expectedTestCount
                <> " tests but getting "
                <> show len
  _ -> error "Parse error."

{-
  the format for a full test case is:
  - first line begins with "= ", followed by a test label (anything but newline),
    then a newline
  - exact same format as a board
  - "----\n"
  - exact same format as a board, but without the row and col count.
 -}
fullTestCaseP :: ReadP (String, (TmpBoard, TmpBoard))
fullTestCaseP = do
  label <- string "= " *> munch1 (/= '\n') <* char '\n'
  bdBefore@(dims, _, _) <- fullBoardP
  _ <- string "----\n"
  bdAfter <- boardP dims
  pure (label, (bdBefore, bdAfter))

spec :: Spec
spec =
  describe "mkBoard & solveBoard" $ forM_ tests $
    \(label, (bdBeforeTmp, bdAfterTmp)) ->
      specify ("Label " <> label) $ do
        let Just bdSolved = do
              (xs, bd) <- mkBoard bdBeforeTmp
              solveBoard bd xs
            Just bdAfter =
              -- as we only care about the parsed board,
              -- we don't need to do anything with bdCandidates.
              snd <$> mkBoard bdAfterTmp
        pending
