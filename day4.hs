totalOverlap :: String -> Int
totalOverlap s = foldr ((+) . boolToInt . doesOverlap . pairs) 0 (lines s)

doesOverlap :: ((Int, Int), (Int, Int)) -> Bool
doesOverlap ((e1Start, e1End), (e2Start, e2End)) = o
  where
    o = e1Bigger || e2Bigger
    e1Bigger = e1Start <= e2Start && e1End >= e2End
    e2Bigger = e2Start <= e1Start && e2End >= e1End

pairs :: String -> ((Int, Int), (Int, Int))
pairs s = ((read e1Start, read e1End), (read e2Start, read e2End))
  where
    (e1Start, e1End) = split ('-' ==) e1
    (e2Start, e2End) = split ('-' ==) e2
    (e1, e2) = split (',' ==) s

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left, right)
  where
    (left, right') = break f s
    right = if null right' then [] else tail right'

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

test = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

run = do
  let file = "input.txt"
  contents <- readFile file
  print $ totalOverlap2 contents

totalOverlap2 :: String -> Int
totalOverlap2 s = foldr ((+) . boolToInt . doesOverlap2 . pairs) 0 (lines s)

doesOverlap2 :: ((Int, Int), (Int, Int)) -> Bool
doesOverlap2 ((e1Start, e1End), (e2Start, e2End)) = o
  where
    o = e1o || e2o
    e1o = (e1Start >= e2Start && e1Start <= e2End) || (e1End <= e2End && e1End >= e2Start)
    e2o = (e2Start >= e1Start && e2Start <= e1End) || (e2End <= e1End && e2End >= e1Start)