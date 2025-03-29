module Game.Minesweeper.Pretty where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Game.Minesweeper.Solver
import Game.Minesweeper.Types
import System.Console.Terminfo

pprBoard :: Terminal -> Bool -> Board -> IO ()
pprBoard term extraInfo bd@Board {bdDims = (rows, cols), bdNums, bdCandidates} = do
  let fgColor = fromMaybe (flip const) (getCapability term withForegroundColor)
      bgColor = fromMaybe (flip const) (getCapability term withBackgroundColor)
  putStrLn "===="
  forM_ [0 .. rows - 1] $ \r -> do
    rs <- forM [0 .. cols - 1] $ \c -> do
      let coord = (r, c)
      pure $ case getTile bd coord of
        Nothing -> bgColor Green $ fgColor White $ termText "?"
        Just b ->
          if b
            then fgColor Red $ termText "*"
            else
              let content =
                    termText $ maybe " " show (bdNums M.!? coord)
               in case bdCandidates M.!? coord of
                    Nothing ->
                      content
                    Just _ ->
                      bgColor Cyan . fgColor White $ content
    runTermOutput term (mconcat rs)
    putStrLn ""
  putStrLn "===="
  when (extraInfo && not (M.null bdCandidates)) $ do
    putStrLn "Candidates:"
    forM_ (M.toAscList bdCandidates) $
      \(coord, cs) -> do
        let n = bdNums M.! coord
        putStrLn $ show n <> " on " <> show coord <> ":"
        mapM_ (putStrLn . ("  " ++) . show) cs
