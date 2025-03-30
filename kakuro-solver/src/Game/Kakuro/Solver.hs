module Game.Kakuro.Solver
  ( nakedPairsElim
  , nakedTriplesElim
  , solve
  , isSolved
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer.CPS
import qualified Data.DList as DL
import Data.Function
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Game.Kakuro.DigitSet as DS
import Game.Kakuro.Types

{-
  Result type of a non-branching step.

  `Nothing` if input `SolState` is found impossible to solve,
  otherwise resulting `SolState` is returned together with a queue
  with coords that are updated in any form.

  Note that a simple Maybe type is not sufficient, as we need the
  distinction between:

  - an impossible puzzle
  - a puzzle that is still possible but current function cannot find any improvement.

 -}
type StepResult = Maybe (SolState, Seq.Seq Coord)

{-
  Refinements that does not rely on brute force:

  Here we maintain a queue of `Coord`s, which is initially populated
  by all unknown cells. This queue is then processed by applying
  different non-brancing strategies in a specific order.
  If any improvements are made (i.e. result in some clues' or cells' candiates being updated),
  those are added to the queue for later.

 -}

refineByCandidates :: SolState -> Maybe SolState
refineByCandidates ss0 = refineQueued ss0 (Seq.fromList (M.keys $ ssCandidates ss0))

refineQueued :: SolState -> Seq.Seq Coord -> Maybe SolState
refineQueued ss q = case q of
  Seq.Empty -> Just ss
  cur Seq.:<| q' ->
    if M.member cur (ssCandidates ss)
      then do
        (ss1, q1) <- stepCluesToCoord cur ss
        (ss2, q2) <- stepCoordToClues cur ss1
        (ss3, q3) <- dischargeCoord cur ss2
        refineQueued ss3 (q1 <> q2 <> q3 <> q')
      else refineQueued ss q'

{-
  Given a coord as input, try all its candidates, and then `refine`.

  - if only one candidate survives, we know this is the only way forward.
  - if none survive, we know the puzzle is impossible
  - if multiple survives, we need some logic to summarize what we know from
    each branch and somehow reflect those information back.
 -}
stepBruteForce :: Coord -> SolState -> Maybe (Either [SolState] SolState)
stepBruteForce cur ss = case ssCandidates ss M.!? cur of
  Nothing ->
    -- target cell already solved.
    Just (Right ss)
  Just cans ->
    let alts = do
          v <- DS.toList cans
          let ss1 = ss {ssCandidates = M.insert cur (DS.singleton v) (ssCandidates ss)}
          Just ss2 <- pure $ refineQueued ss1 (Seq.singleton cur)
          pure ss2
     in case alts of
          [] -> Nothing
          [x] -> Just (Right x)
          xs@(_ : _ : _) -> Just (Left xs)

{-
  This is likely our last resort: just set the cell to every possible value,
  and see what can we learn from resulting states.

  This should probably be called outside of `refine`, otherwise
  it may become a dead loop (refine -> shallowBruteForce -> refine -> ...).

 -}
shallowBruteForce :: Coord -> SolState -> Maybe SolState
shallowBruteForce cur ss = do
  r <- stepBruteForce cur ss
  case r of
    Left vs -> do
      let
        reconsCandidates :: SolState -> M.Map Coord DS.DigitSet
        reconsCandidates sf =
          M.mapWithKey
            ( \k _ -> case ssCandidates sf M.!? k of
                Nothing -> DS.singleton (ssSolved sf M.! k)
                Just v -> v
            )
            (ssCandidates ss)
        refinedCandidates = M.unionsWith DS.union (fmap reconsCandidates vs)
      let
        changedCoords :: [Coord]
        changedCoords = do
          (k, canBefore) <- M.toList (ssCandidates ss)
          let canAfter = refinedCandidates M.! k
          guard (canBefore /= canAfter)
          pure k
      case changedCoords of
        [] -> pure ss
        _ : _ -> do
          let ss' = ss {ssCandidates = refinedCandidates}
          Just ss'' <- pure $ refineQueued ss' (Seq.fromList changedCoords)
          pure ss''
    Right ss' ->
      pure ss'

{-
  Given a cell (by its Coord), there are clues associated with it.
  This function attempts to use clues' candidates to intersect that of cell's.

  `Nothing` if any inconsistency / contradiction found,
  otherwise returns updated `SolState` and whether any changes are made.
 -}
stepCluesToCoord :: Coord -> SolState -> StepResult
stepCluesToCoord cur ss@SolState {ssCandidates, ssClues, ssCoordToClue} = do
  r <-
    M.alterF
      ( \case
          Nothing ->
            -- fail when key is missing
            Nothing
          Just cans -> do
            let cans' =
                  -- TODO: impl DS.intersections?
                  -- merge by intersect because we have to satisfy all clues
                  foldr DS.intersection cans clues
            guard $ not (DS.null cans')
            pure (Just cans')
      )
      cur
      ssCandidates
  let nexts =
        if DS.size (ssCandidates M.! cur) /= DS.size (r M.! cur)
          then Seq.singleton cur
          else Seq.empty
  pure (ss {ssCandidates = r}, nexts)
  where
    clues :: [DS.DigitSet]
    clues =
      mapMaybe
        ( \ci -> do
            (_, alts) <- ssClues M.!? ci
            -- merge by union as those represent alternatives
            pure $ DS.unions alts
        )
        (ssCoordToClue M.! cur)

{-
  Given a cell (by its Coord), there are clues associated with it.
  This function uses cell's candidates to intersect that of clues'.

  `Nothing` if any inconsistency / contradiction found,
  otherwise returns updated `SolState` and whether any changes are made.
 -}
stepCoordToClues :: Coord -> SolState -> StepResult
stepCoordToClues cur ss@SolState {ssCandidates, ssClues, ssCoordToClue} = do
  let clueInds = ssCoordToClue M.! cur
  cans <- ssCandidates M.!? cur
  ssClues' <-
    foldM
      ( \m ci ->
          M.alterF
            ( \case
                Nothing ->
                  -- missing key is fine, probably already discharged
                  pure Nothing
                Just (s, alts) -> do
                  -- here we assume it is non-empty to begin with
                  let alts' = filter (\a -> not $ DS.null (DS.intersection a cans)) alts
                  guard $ not (null alts')
                  pure $ Just (s, alts')
            )
            ci
            m
      )
      ssClues
      clueInds
  let nexts =
        Seq.fromList
          . S.toList
          . S.delete cur
          $ foldMap
            ( \ci -> case ssClues' M.!? ci of
                Nothing -> mempty
                Just (s, alts') ->
                  let (_, alts) = ssClues M.! ci
                   in if alts == alts' then mempty else s
            )
            clueInds
  pure (ss {ssClues = ssClues'}, nexts)

dischargeCoord :: Coord -> SolState -> StepResult
dischargeCoord cur ss@SolState {ssSolved, ssCandidates, ssClues, ssCoordToClue} = do
  cans <- ssCandidates M.!? cur
  case DS.toList cans of
    [] -> Nothing
    [v] -> do
      let clueInds = ssCoordToClue M.! cur
      ssClues' <-
        foldM
          ( \m ci ->
              M.alterF
                ( \case
                    Nothing ->
                      pure Nothing
                    Just (s, alts) -> do
                      let alts' =
                            concatMap
                              ( \a ->
                                  if DS.member v a
                                    then [DS.delete v a]
                                    else []
                              )
                              alts
                      guard $ not (null alts')
                      pure $ Just (S.delete cur s, alts')
                )
                ci
                m
          )
          ssClues
          clueInds
      let nexts =
            Seq.fromList
              . S.toList
              $ foldMap
                ( \ci -> case ssClues' M.!? ci of
                    Nothing -> mempty
                    Just (s, alts') ->
                      let (_, alts) = ssClues M.! ci
                       in if alts == alts' then mempty else s
                )
                clueInds
      pure
        ( ss
            { ssSolved = M.insert cur v ssSolved
            , ssCandidates = M.delete cur ssCandidates
            , ssClues = ssClues'
            }
        , nexts
        )
    _ : _ : _ -> pure (ss, Seq.empty)

-- https://hackage.haskell.org/package/combinat-0.2.10.1/docs/src/Math.Combinat.Sets.html#choose
{-
  for detecting naked pair / triple.
 -}
choose :: Int -> [a] -> DL.DList [a]
choose = \cases
  0 _ -> DL.singleton []
  _ [] -> DL.empty
  k (x : xs) -> fmap (x :) (choose (k - 1) xs) <> choose k xs

{-
  Note that we are not just finding naked pairs / tuples, it is important to also
  make sure that we make progress by eliminate those elements from other candidate lists.
  Without this guarantee we would just end up finding the same pair over and over again
  that cannot help with reducing # of candidates.
 -}

type NakedTupleEliminator eid =
  [(eid, DS.DigitSet)] ->
  []
    ( NE.NonEmpty (eid, DS.DigitSet) -- a naked tuple found
    , NE.NonEmpty (eid, DS.DigitSet) -- other elements being affected (don't put unaffected stuff here)
    )

nakedPairsElim :: Eq a => NakedTupleEliminator a
nakedPairsElim = \case
  [] -> []
  [_, _] ->
    -- since the purpose is to reduce candidates in other cells, we can skip the case where l <= 2
    []
  xs0 -> do
    let xs1 = filter ((== 2) . DS.size . snd) xs0
    guard $ not (null xs1)
    cs@[(ia, a), (ib, b)] <- DL.toList $ choose 2 xs1
    guard $ a == b

    Just rs <- pure $ NE.nonEmpty do
      concatMap
        ( \(i, s) ->
            do
              guard $ i /= ia && i /= ib
              let s' = DS.difference s a
              guard $ s' /= s
              pure (i, s')
        )
        xs0
    pure (NE.fromList cs, rs)

nakedTriplesElim :: Eq a => NakedTupleEliminator a
nakedTriplesElim = \case
  [] -> []
  [_] -> []
  [_, _] -> []
  [_, _, _] ->
    -- since the purpose is to reduce candidates in other cells, we can skip the case where l <= 3
    []
  xs0 -> do
    let xs1 = filter ((\i -> i == 2 || i == 3) . DS.size . snd) xs0
    guard $ not (null xs1)
    cs@[(ia, a), (ib, b), (ic, c)] <- DL.toList $ choose 3 xs1
    let fullSet = DS.union a (DS.union b c)
    guard $ DS.size fullSet == 3

    Just rs <- pure $ NE.nonEmpty do
      concatMap
        ( \(i, s) ->
            do
              guard $ i /= ia && i /= ib && i /= ic
              let s' = DS.difference s fullSet
              guard $ s' /= s
              pure (i, s')
        )
        xs0

    pure (NE.fromList cs, rs)

refineByClue ::
  NakedTupleEliminator Coord ->
  ClueInd ->
  SolState ->
  StepResult
refineByClue strat ci ss = case ssClues ss M.!? ci of
  Nothing -> Just (ss, Seq.empty)
  Just (s :: S.Set Coord, _) -> do
    let
      xs = S.toList s
      cs :: [DS.DigitSet]
      cs = fmap (\c -> ssCandidates ss M.! c) xs
    case strat (zip xs cs) of
      [] -> pure (ss, Seq.empty)
      (_, upd0) : _ -> do
        let
          upd1 :: [(Coord, DS.DigitSet)]
          upd1 = NE.toList upd0
          ssCandidates' = foldr (\(i, v) r -> M.insert i v r) (ssCandidates ss) upd1
        pure (ss {ssCandidates = ssCandidates'}, Seq.fromList xs)

refineByClues :: SolState -> StepResult
refineByClues ss0 = do
  runWriterT $
    foldM
      ( \ss1 i -> do
          (ss2, q2) <- lift $ refineByClue nakedPairsElim i ss1
          tell q2
          (ss3, q3) <- lift $ refineByClue nakedTriplesElim i ss2
          tell q3
          pure ss3
      )
      ss0
      (M.keys (ssClues ss0))

refineAll :: SolState -> Maybe SolState
refineAll ss0 = do
  {-
    If our algorithm ends up repeating unnecessary searches,
    `refineByClues` may end up not justifying running it.

    If one find setting this to False makes it run faster,
    chances are we are trying too many tactics that doesn't
    make too much progress.
   -}
  let useRefineByClues = True
  ss1 <- refineByCandidates ss0

  if useRefineByClues
    then do
      (ss2, q) <- refineByClues ss1
      refineQueued ss2 q
    else pure ss1

{-
  Current strat:

  - `refineAll` to exhaust impossible alts
  - `shallowBruteForce` to try probing all alts of a cell
    and see if we can infer something from all proceeding states.
  - repeat until fixpoint.

 -}
solve :: SolState -> Maybe SolState
solve ss0 =
  refineAll ss0
    >>= fix
      ( \loop ssCur -> do
          ssNxt <-
            foldM
              (\sNow cur -> shallowBruteForce cur sNow)
              ssCur
              (M.keys (ssCandidates ssCur))
          if ssNxt == ssCur
            then pure ssCur
            else loop ssNxt
      )

{-
  TODO: do we need test coverage to see when we say it's solved, is it actually?
 -}
isSolved :: SolState -> Bool
isSolved = M.null . ssCandidates
