most :: String -> Int
most = foldr (max . sum) 0 . split "" [] . lines

split :: String -> [Int] -> [String] -> [[Int]]
split div acc [] = [acc]
split div acc (x : xs) = x == div ? acc : split div [] xs ?: split div (read x : acc) xs

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

most3 :: String -> Int
most3 = sum3 . foldr (max3 . sum) (0, 0, 0) . split "" [] . lines

sum3 (a, b, c) = a + b + c

max3 x (a, b, c) = (a', b', c')
  where
    a' = x > a ? x ?: a
    b' = x <= a && x > b ? x ?: (x > a ? a ?: b)
    c' = x <= b && x > c ? x ?: (x > b ? b ?: c)