startConf =
  [ ['N', 'Q', 'L', 'S', 'C', 'Z', 'P', 'T'],
    ['G', 'C', 'H', 'V', 'T', 'P', 'L'],
    ['F', 'Z', 'C', 'D'],
    ['C', 'V', 'M', 'L', 'D', 'T', 'W', 'G'],
    ['C', 'W', 'P'],
    ['Z', 'S', 'T', 'C', 'D', 'J', 'F', 'P'],
    ['D', 'B', 'G', 'W', 'V'],
    ['W', 'H', 'Q', 'S', 'J', 'N'],
    ['V', 'L', 'S', 'F', 'Q', 'C', 'R']
  ]

allMoved :: String -> String
allMoved s = foldr ((:) . head) "" moved
  where
    moved = foldl (\b a -> move (parseLine a) b) startConf (lines s)

parseLine :: String -> (Int, Int, Int)
parseLine s = (read $ splat !! 1, read $ splat !! 3, read $ splat !! 5)
  where
    splat = words s

move :: (Int, Int, Int) -> [[Char]] -> [[Char]]
move (1, from, to) xs = zipWith (\i x -> i == from || i == to ? (i == from ? tail x ?: head (xs !! (from - 1)) : x) ?: x) [1 ..] xs
move (count, from, to) xs = move (count - 1, from, to) $ move (1, from, to) xs

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

test = "move 6 from 2 to 1\nmove 4 from 6 to 3\nmove 1 from 6 to 5\nmove 8 from 3 to 8\nmove 13 from 8 to 2"

run = do
  let file = "input.txt"
  contents <- readFile file
  print $ allMoved contents

move2 :: (Int, Int, Int) -> [[Char]] -> [[Char]]
move2 (count, from, to) xs = zipWith (\i x -> i == from || i == to ? (i == from ? drop count x ?: take count (xs !! (from - 1)) ++ x) ?: x) [1 ..] xs
