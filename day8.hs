import Data.Char
import Data.List
import Data.Maybe

treetretree :: String -> Int
treetretree = length . concat . filterHidden . parseGrid . lines

parseGrid :: [String] -> [[Int]]
parseGrid = map (map digitToInt)

filterHidden :: [[Int]] -> [[Int]]
filterHidden xs = rowsFiltered
  where
    rowsFiltered = map (map snd . filterRow transposed) (index rows)
    transposed = index $ map index $ transpose xs
    rows = map index xs

filterRow :: [(Int, [(Int, Int)])] -> (Int, [(Int, Int)]) -> [(Int, Int)]
filterRow ts (ri, r) =
  filter
    ( \(i, k) ->
        testFront (ri, r) (i, k)
          || testBack (ri, r) (i, k)
          || testFront (ts !! i) (ri, k)
          || testBack (ts !! i) (ri, k)
    )
    r

testFront :: (Int, [(Int, Int)]) -> (Int, Int) -> Bool
testFront (ri, rs) (i, x) = foldr (\(_, k) acc -> (k < x) && acc) True (takeWhile (\(z, _) -> z < i) rs)

testBack :: (Int, [(Int, Int)]) -> (Int, Int) -> Bool
testBack (ri, rs) (i, x) = foldr (\(_, k) acc -> (k < x) && acc) True (dropWhile (\(z, _) -> z <= i) rs)

index :: [a] -> [(Int, a)]
index = zip [0 ..]

run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  print $ treetretree2 contents

treetretree2 :: String -> Int
treetretree2 = foldr max 0 . concat . mapSight . parseGrid . lines

mapSight :: [[Int]] -> [[Int]]
mapSight xs = mapped
  where
    mapped = map (mapRow transposed) (index rows)
    transposed = index $ map index $ transpose xs
    rows = map index xs

mapRow :: [(Int, [(Int, Int)])] -> (Int, [(Int, Int)]) -> [Int]
mapRow ts (ri, r) =
  map
    ( \(i, k) ->
        sightFront (ri, r) (i, k)
          * sightBack (ri, r) (i, k)
          * sightFront (ts !! i) (ri, k)
          * sightBack (ts !! i) (ri, k)
    )
    r

sightFront :: (Int, [(Int, Int)]) -> (Int, Int) -> Int
sightFront (ri, rs) (i, x) = (i -) $ fst $ fromMaybe (0, 0) $ find (\(ki, k) -> k >= x) (reverse $ takeWhile (\(z, _) -> z < i) rs)

sightBack :: (Int, [(Int, Int)]) -> (Int, Int) -> Int
sightBack (ri, rs) (i, x) = (\a -> a - i) $ fst $ fromMaybe (length rs - 1, 0) $ find (\(ki, k) -> k >= x) (dropWhile (\(z, _) -> z <= i) rs)

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?: