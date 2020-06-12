module Main
  ( main,
  )
where

type Coord = (Int, Int)

{- ORMOLU_DISABLE -}
-- 2d offset of 8 surrounding tiles.
surroundings :: [(Int, Int)]
surroundings =
  [ (-1, -1), (-1, 0), (-1, 1)
  , (0, -1), (0, 1)
  , (1, -1), (1, 0), (1, 1)
  ]
{- ORMOLU_ENABLE -}

main :: IO ()
main = pure ()
