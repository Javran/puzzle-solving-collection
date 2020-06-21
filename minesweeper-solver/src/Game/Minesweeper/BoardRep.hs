module Game.Minesweeper.BoardRep
  ( isPartial,
    mergeBoardRep,
  )
where

import Control.Monad
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Game.Minesweeper.Types

isPartial :: BoardRep -> Bool
isPartial = not . S.null . brMissing

{-
  TODO: should be able to use mergeA to simplify this, and also make some unit tests.
 -}

consistentMerge :: (Ord k, Eq v) => M.Map k v -> M.Map k v -> Maybe (M.Map k v)
consistentMerge l r =
  M.union l r
    <$ sequence_
      ( M.intersectionWith (\a b -> () <$ guard (a == b)) l r
      )

mergeBoardRep :: BoardRep -> BoardRep -> Maybe BoardRep
mergeBoardRep brNew brCur = do
  guard $ ((==) `on` brDims) brNew brCur
  nums <- (consistentMerge `on` brNums) brNew brCur
  mines <- (consistentMerge `on` brMines) brNew brCur
  pure
    brCur
      { brNums = nums,
        brMines = mines,
        brMissing = (S.intersection `on` brMissing) brNew brCur
      }
