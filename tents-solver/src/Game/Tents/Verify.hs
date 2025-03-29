module Game.Tents.Verify
  ( verifyBoard
  )
where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Game.Tents.Types

{-
  Verify that a game board is solved
 -}

{-
  In general we don't want to do a search for maximum bipartite matching for performance concerns,
  but in this case this is fine since one tree gets only up to 4 potential tent matches,
  the branching factor is quite small to begin with.
  Plus on the use site, we only need to find one full match to verify a solution.
 -}
doMatch :: M.Map Coord [Coord] -> [] [(Coord, Coord)]
doMatch m =
  doMatchAux
    S.empty
    -- by sorting we make sure that unique pairs are resolved first to reduce branching factor.
    (sortOn (length . snd) $ M.toList m)
  where
    doMatchAux tentsTaken todoTreeTentPairs =
      case todoTreeTentPairs of
        [] -> pure []
        (treeCoord, tentCoordsPre) : todo' -> do
          let tentCoords = filter (`notElem` tentsTaken) tentCoordsPre
          tentCoord <- tentCoords
          ((treeCoord, tentCoord) :) <$> doMatchAux (S.insert tentCoord tentsTaken) todo'

verifyBoard :: (Int, Int) -> M.Map Coord Cell -> Bool
verifyBoard dims cells = isJust $ do
  let allCoords =
        S.fromList
          [ (r, c)
          | r <- [0 .. rows - 1]
          , c <- [0 .. cols - 1]
          ]
      (treeCoords, tentCoords) = foldMap go (M.toList cells)
        where
          go (x, Tent) = (S.empty, S.singleton x)
          go (x, Tree) = (S.singleton x, S.empty)
          go _ = (S.empty, S.empty)
      surroundingCells (r, c) =
        S.fromList
          [ (r + dr, c + dc)
          | dr <- [-1, 0, 1]
          , dc <- [-1, 0, 1]
          , (dr, dc) /= (0, 0)
          ]
      altTreeTentPairs :: M.Map Coord [Coord]
      altTreeTentPairs = M.fromList . fmap mkPair . S.toList $ treeCoords
        where
          mkPair :: Coord -> (Coord, [Coord])
          mkPair coord@(r, c) = (coord, pairingTents)
            where
              pairingTents = [coord' | coord' <- possibleTentCoords, coord' `elem` tentCoords]
              possibleTentCoords =
                {-
                  those might be out-of-bound, but since we are just looking for matches
                  in region already constrainted by dims, there is no impact in terms of correctness.
                 -}
                [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

  -- make sure all coords are assigned Cell values.
  guard $ allCoords == M.keysSet cells
  -- make sure the count is correct to make pairs.
  -- guard $ S.size treeCoords == S.size tentCoords
  -- tents should not near each other.
  guard $
    all
      (\coord -> S.null (S.intersection (surroundingCells coord) tentCoords))
      tentCoords
  {-
    each tree must pair with exactly one tent.
    altTreeTentPairs shows all possible tents one tree can be pair with.
    Here we first verify that all trees have at least one tent to be paired with.
   -}
  guard $
    all
      (not . null)
      (M.elems altTreeTentPairs)
  -- as long as we can establish one valid full match, this solution is valid.
  listToMaybe $ doMatch altTreeTentPairs
  where
    (rows, cols) = dims
