import Data.List
import Data.Set hiding (drop, map)

countNoBeacon = length . unions . map (noBeacon 2000000 . parseline) . lines

{- noBeacon :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
noBeacon ((sx, sy), (bx, by)) = [(sx + dx, sy + dy) | dx <- [-dist .. dist], dy <- [-dist .. dist], abs dy + abs dx <= dist, sy + dy == 2000000]
  where
    dist = abs (sx - bx) + abs (sy - by) -}

noBeacon' :: Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
noBeacon' row ((sx, sy), (bx, by)) = dy > row ? [] ?: positions
  where
    dist = abs (sx - bx) + abs (sy - by)
    dy = abs $ row - sy
    positions = [(sx + dx, row) | dx <- [-dist + dy .. dist - dy], not (sx + dx == bx && row == by)]

noBeacon :: Int -> ((Int, Int), (Int, Int)) -> Set (Int, Int)
noBeacon row ((sx, sy), (bx, by)) = positions
  where
    dist = abs (sx - bx) + abs (sy - by)
    dy = abs $ row - sy
    positions = fromList [(sx + dx, row) | dx <- [-dist + dy .. dist - dy], not (sx + dx == bx && row == by)]

parseline :: String -> ((Int, Int), (Int, Int))
parseline s = ((sx, sy), (bx, by))
  where
    sx = read $ init $ drop 2 (wordss !! 2)
    sy = read $ init $ drop 2 (wordss !! 3)
    bx = read $ init $ drop 2 (wordss !! 8)
    by = read $ drop 2 (wordss !! 9)
    wordss = words s

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

main :: IO ()
main = do
  let file = "input.txt"
  contents <- readFile file
  let output = findBeacon contents
  print output

findBeacon s = Data.List.filter (\p -> all (furtherAway p) sb) pa
  where
    pa = concat. map getPointsAround $ sb
    sb = map parseline . lines $ s

furtherAway :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
furtherAway (px, py) ((sx, sy), (bx, by)) = pdist > bdist
  where
    pdist = abs (sx - px) + abs (sy - py)
    bdist = abs (sx - bx) + abs (sy - by)

getPointsAround :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getPointsAround ((sx, sy), (bx, by)) = [(sx + d, sy + (abs d - dist) * n) | d <- [-dist .. dist], n <- [-1, 1], sx + d >= 0 && sx + d <= 4000000, sy + (abs d - dist) * n >= 0 && sy + (abs d - dist) * n <= 4000000]
  where
    dist = abs (sx - bx) + abs (sy - by) + 1