{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Arrow.Gaussian
  ( solveMat
  , solveMatOne
  , shuffler
  )
where

import Control.Monad
import Control.Monad.Loops
import Data.Bifunctor
import Data.List
import Data.List.HT
import Data.Monoid
import Game.Arrow.Types

-- expect both input to be positive numbers.
extEuclidean :: Integral i => i -> i -> (i, (i, i))
extEuclidean a0 b0 = aux (a0, 1, 0) (b0, 0, 1)
  where
    aux (r0, s0, t0) y@(r1, s1, t1) =
      if r1 == 0
        then (r0, (s0, t0))
        else aux y (r, s0 - q * s1, t0 - q * t1)
      where
        (q, r) = r0 `quotRem` r1

-- computes multiplicative inverse modulo p.
-- returns input value on failure.
multInv :: Integral i => i -> i -> Either i i
multInv p x =
  if comm == 1
    then
      Right $
        -- p * s + x * t = 1
        t `mod` p
    else Left x
  where
    (comm, (_s, t)) = extEuclidean p x

type ElimStepM i = [[i]] -> Either (Err i) (Maybe ([i], [[i]]))

solveMat' :: Integral i => (i -> ElimStepM i) -> i -> [[i]] -> Either (Err i) [i]
solveMat' fallback m mat = do
  ut <- upperTriangular fallback m mat
  pure $ reverse $ unfoldr (solveStep m) ([], reverse ut)

solveMat :: Integral i => i -> [[i]] -> Either (Err i) [i]
solveMat = solveMat' (\_ _ -> Left Underdetermined)

solveMatOne :: Integral i => i -> [[i]] -> Either (Err i) [i]
solveMatOne = solveMat' underDetFallback

-- try to get one solution out of an underdetermined system.
underDetFallback :: Integral i => i -> ElimStepM i
underDetFallback m eqns
  | null eqns = stop
  | isLhsSquare = do
    {-
      note that here we also have l >= 1.
     -}
    let fstNonZeroInd =
          getFirst
            . mconcat
            . fmap (First . findIndex (\v -> v `mod` m /= 0))
            $ eqns
    case fstNonZeroInd of
      Nothing ->
        {-
          Making this determined by fixing underdetermined variables to 0.
         -}
        let (hdL : tlL) =
              fmap
                (<> [0])
                [[if r == c then 1 else 0 | c <- [1 .. l]] | r <- [1 .. l]]
         in Right $ Just (hdL, fmap (drop 1) tlL)
      Just nzInd -> do
        let common = foldr gcd m (concat eqns)
            m' = m `div` common
            eqns' = (fmap . fmap) (`div` common) eqns
            (doShuffle, unShuffle) = shuffler nzInd
            eqns'' = fmap doShuffle eqns'
        {-
          This might seem to be a potential for infinite loop,
          but we have prevented that by moving a column that contains
          non-zero element to the front to make sure it's making progress.

          Update: another case is when gcd is 1, in which case we leave it unhandled for now.
         -}
        when (common == 1) $
          Left $ Todo (show nzInd)
        rsPre <- solveMatOne m' eqns''
        let rs = unShuffle rsPre
            (hdL : tlL) =
              zipWith
                (\r lhs -> lhs <> [r])
                rs
                [[if r == c then 1 else 0 | c <- [1 .. l]] | r <- [1 .. l]]
        Right $ Just (hdL, fmap (drop 1) tlL)
  | otherwise = stop
  where
    stop = Left Underdetermined
    l = length eqns
    isLhsSquare = all ((== l + 1) . length) eqns

upperTriangular
  :: forall i.
  Integral i
  => (i -> ElimStepM i)
  -> i
  -> [[i]]
  -> Either (Err i) [[i]]
upperTriangular fallback m = unfoldrM elimStepM
  where
    elimStepM :: ElimStepM i
    elimStepM eqns = do
      let alts = do
            -- any equation without a zero on front
            (e@(hd : _), es) <- removeEach eqns
            guard $ gcd m hd == 1
            pure (e, es)
      case alts of
        [] ->
          if null eqns
            then Right Nothing
            else fallback m eqns
        ([], _) : _ -> Left Underdetermined
        (e@(hd : _), es) : _ -> do
          invHd <- first NoMultInv $ multInv m hd
          let mul x y = (x * y) `mod` m
              eNorm = fmap (mul invHd) (e :: [i])
              norm eqn@(eh : _) =
                if eh == 0
                  then eqn
                  else zipWith (\a b -> (a - b) `mod` m) eqn (fmap (mul eh) eNorm)
              norm _ = error "length not unique"
          pure $ Just (eNorm, fmap (drop 1 . norm) es)

solveStep :: Integral i => i -> ([i], [[i]]) -> Maybe (i, ([i], [[i]]))
solveStep _ (_, []) = Nothing
solveStep m (xs, hd : tl) = do
  let x = (rhs - sum (zipWith (*) lhs xs)) `mod` m
      1 : ys = hd
      rhs = last ys
      lhs = init ys
  pure (x, (x : xs, tl))

shuffler :: Int -> ([a] -> [a], [a] -> [a])
shuffler i =
  if i <= 0
    then (id, id)
    else
      let doShuffle xs =
            if i >= l
              then xs
              else
                let (pres, h : tl) = splitAt i xs
                 in h : pres <> tl
            where
              l = length xs
          unShuffle xs =
            if i >= l
              then xs
              else
                let (hd : ys) = xs
                    (ys0, ys1) = splitAt i ys
                 in ys0 <> (hd : ys1)
            where
              l = length xs
       in (doShuffle, unShuffle)
