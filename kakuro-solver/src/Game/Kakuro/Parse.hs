module Game.Kakuro.Parse
  ( fromYaml
  , PuzzleRaw (..)
  , puzzleFromRaw
  , puzzleToSolState
  ) where

{-
  Parses from YAML format and prepares for solving.

  The YAML file should be a single Object with following fields:

  - name: string, not used, just for annotations.

  - grid: multiline string, `.` denotes empty cells to be solved,
    other characters are ignored.

  - clues: an Object containing `rows` and `cols` fields.

    + rows: every element corresponds to a row (starting from the first row
      that contains an unsolved cell) in `grid`, from top to bottom.
      Each element is an array of numbers, each number denotes the clue sum for
      a consecutive chunk of unsolved cells.
      If there are rows that contain nothing (which should usually not be the case
      since the implication is we are dealing with two independent puzzles),
      use an empty array as placeholder.

    + cols: exactly the same as `rows`, but scans left-to-right by column.
 -}

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.DList as DL
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple
import qualified Data.Yaml as Yaml
import qualified Game.Kakuro.DigitSet as DS
import Game.Kakuro.Types

{-
  Intermediate data type meant to simply receive
  information from a raw file.
 -}
data PuzzleRaw = PuzzleRaw
  { prName :: T.Text
  , prGrid :: T.Text
  , prClueRows :: [] [Int]
  , prClueCols :: [] [Int]
  }
  deriving (Show)

instance FromJSON PuzzleRaw where
  parseJSON = withObject "PuzzleRaw" \o -> do
    prName <- o .: "name"
    prGrid <- o .: "grid"
    clues :: Object <- o .: "clues"
    prClueRows <- clues .: "rows"
    prClueCols <- clues .: "cols"
    pure PuzzleRaw {prName, prGrid, prClueRows, prClueCols}

instance ToJSON PuzzleRaw where
  toJSON PuzzleRaw {prName, prGrid, prClueRows, prClueCols} =
    object
      [ "name" .= prName
      , "grid" .= prGrid
      , "clues"
          .= object
            [ "rows" .= prClueRows
            , "cols" .= prClueCols
            ]
      ]

groupCoords :: Dir -> [Coord] -> [NE.NonEmpty Coord]
groupCoords d = fix $ \go -> \case
  [] -> []
  xs@(hd : _) ->
    let
      consecs = iterate (applyDir d) hd
      zipped = zip xs consecs
      (ls, rs) = span (uncurry (==)) zipped
     in
      NE.fromList (fmap fst ls) : go (fmap fst rs)

fromYaml :: FilePath -> IO (Either String PuzzleRaw)
fromYaml fp =
  Yaml.decodeFileEither @Value fp >>= \case
    Left err -> pure $ Left (displayException err)
    Right r ->
      case parseEither (parseJSON @PuzzleRaw) r of
        Left err -> pure $ Left err
        Right r1 -> pure $ Right r1

puzzleFromRaw :: PuzzleRaw -> Either String (Puzzle, SolState)
puzzleFromRaw PuzzleRaw {prGrid, prClueRows, prClueCols} = do
  let
    gridLines = T.unpack <$> T.lines prGrid
    cellCoords :: [Coord]
    cellCoords = do
      (r, ln) <- zip [0 ..] gridLines
      (c, ch) <- zip [0 ..] ln
      guard $ ch == '.'
      pure (r, c)

  pzBounds <- case foldMap (Just . minMax2D) cellCoords of
    Nothing -> Left "No coords"
    Just v -> pure v

  {-
    Technically we can flatten things and get clues paired,
    but here we choose to traverse multiple dimensions as
    a way to validate input while we are constructing the actual Puzzle.
   -}

  let
    rowScans :: [] [NE.NonEmpty Coord]
    rowScans =
      groupBy (on (==) (fst . NE.head))
        . groupCoords R
        $ cellCoords
    colScans :: [] [NE.NonEmpty Coord]
    colScans =
      groupBy (on (==) (snd . NE.head))
        . groupCoords D
        $ sortOn swap cellCoords

  unless (length rowScans == length prClueRows) $
    Left "row clue count mismatched (overall)"

  pzCluesHori <-
    M.fromList . DL.toList . mconcat <$> forM (zip rowScans prClueRows) \(cluster, clues) -> do
      -- on current row
      unless (length cluster == length clues) $
        Left "row clue count mismatched (on a row)"
      pure $ DL.fromList (zipWith (\(hd NE.:| tl) cu -> (hd, TotCnt cu (1 + length tl))) cluster clues)

  unless (length colScans == length prClueCols) $
    Left "col clue count mismatched (overall)"

  pzCluesVert <-
    M.fromList . DL.toList . mconcat <$> forM (zip colScans prClueCols) \(cluster, clues) -> do
      -- on current col
      unless (length cluster == length clues) $
        Left "col clue count mismatched (on a col)"
      pure $ DL.fromList (zipWith (\(hd NE.:| tl) cu -> (hd, TotCnt cu (1 + length tl))) cluster clues)

  let
    pzCells = S.fromDistinctAscList cellCoords
    puzzle =
      Puzzle
        { pzCells
        , pzBounds
        , pzCluesHori
        , pzCluesVert
        }
  pure (puzzle, puzzleToSolState puzzle)

puzzleToSolState :: Puzzle -> SolState
puzzleToSolState Puzzle {pzCells, pzCluesHori, pzCluesVert} = do
  let
    allCandi = DS.fromList [1 .. 9]
    (cluesH, coordToClueH) = unzip do
      (orig, TotCnt tot cnt) <- M.toAscList pzCluesHori
      let
        consecs = take cnt $ iterate (applyDir R) orig
        coords = S.fromList consecs
        ci :: ClueInd
        ci = (orig, Hori)
      pure
        ( M.singleton ci (coords, fromMaybe [] $ getCombs (TotCnt tot (S.size coords)))
        , M.fromSet (\_ -> DL.singleton ci) coords
        )
    (cluesV, coordToClueV) = unzip do
      (orig, TotCnt tot cnt) <- M.toAscList pzCluesVert
      let
        consecs = take cnt $ iterate (applyDir D) orig
        coords = S.fromList consecs
        ci :: ClueInd
        ci = (orig, Vert)
      pure
        ( M.singleton ci (coords, fromMaybe [] $ getCombs (TotCnt tot (S.size coords)))
        , M.fromSet (\_ -> DL.singleton ci) coords
        )
  SolState
    { ssSolved = M.empty
    , ssCandidates = M.fromSet (\_ -> allCandi) pzCells
    , ssClues = M.unions (cluesH <> cluesV)
    , ssCoordToClue = M.map DL.toList $ M.unionsWith (<>) (coordToClueH <> coordToClueV)
    }

instance IsPuzzleRep PuzzleRaw where
  repToPuzzle = (fmap . fmap) fst puzzleFromRaw
