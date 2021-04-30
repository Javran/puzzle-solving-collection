{-# LANGUAGE NamedFieldPuns #-}

module Game.Arrow.Solver
  ( solve
  )
where

import Game.Arrow.CoordSystem
import Game.Arrow.Gaussian
import Game.Arrow.Types

solve :: Puzzle -> Either (Err Int) [[Int]]
solve Puzzle {opMod, pzType = (pzShape, sz), grid} =
  withShape
    pzShape
    (\pty -> do
       let inp =
             (fmap . fmap)
               (\v -> (- v) `mod` opMod)
               grid
           (matLhs, _) = gCoords pty sz
           mat = zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)
       case solveMatOne opMod mat of
         Left e -> Left e
         Right xs -> Right $ toChunks pty sz xs)
