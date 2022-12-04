{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

totalPriorities1 :: String -> Int
totalPriorities1 = foldr ((+) . priority . item1) 0 . lines

test1 = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

item1 :: String -> Char
item1 s = common sack1 sack2
  where
    sack1 = take (length s `div` 2) s
    sack2 = drop (length s `div` 2) s

priority :: Char -> Int
priority c = fromMaybe 0 $ elemIndex c " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

common :: String -> String -> Char
common a1 = foldr (\c acc -> elem c a1 ? c ?: acc) ' '

totalPriorities2 = foldr ((+) . priority . item2) 0 . group 3 . lines

item2 :: [String] -> Char
item2 [] = ' '
item2 (x : xs)
  | commonChar == ' ' = item2 xs
  | otherwise = commonChar
  where
    commonChar = foldr (\c acc -> all (elem c) xs ? c ?: acc) ' ' x

group :: Int -> [a] -> [[a]]
group n [] = []
group n xs = take n xs : group n (drop n xs)

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

test2 = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"