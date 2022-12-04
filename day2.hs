{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

totalPoints1 :: String -> Int
totalPoints1 = foldr ((+) . points1) 0 . lines

points1 :: String -> Int
points1 (o : _ : m : _) = outcome o m + value1 m

value1 :: Char -> Int
value1 'X' = 1
value1 'Y' = 2
value1 'Z' = 3
value1 'A' = 1
value1 'B' = 2
value1 'C' = 3
value1 _ = 0

outcome :: Char -> Char -> Int
outcome 'A' 'X' = 3
outcome 'A' 'Y' = 6
outcome 'A' 'Z' = 0
outcome 'B' 'X' = 0
outcome 'B' 'Y' = 3
outcome 'B' 'Z' = 6
outcome 'C' 'X' = 6
outcome 'C' 'Y' = 0
outcome 'C' 'Z' = 3

totalPoints2 :: String -> Int
totalPoints2 = foldr ((+) . points2) 0 . lines

points2 :: String -> Int
points2 (o : _ : m : _) = outcome o mymove + value1 mymove
  where
    mymove = move o m

move :: Char -> Char -> Char
move 'A' 'X' = 'Z'
move 'A' 'Y' = 'X'
move 'A' 'Z' = 'Y'
move 'B' 'X' = 'X'
move 'B' 'Y' = 'Y'
move 'B' 'Z' = 'Z'
move 'C' 'X' = 'Y'
move 'C' 'Y' = 'Z'
move 'C' 'Z' = 'X'
