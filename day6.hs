tune :: String -> Int
tune s = tuneAux s'
  where 
    s' = zip [1..] s

blockSize = 14

tuneAux :: [(Int, Char)] -> Int
tuneAux xs = matches ? tuneAux (tail xs) ?: (blockSize - 1 + (fst $ head xs) )
  where
    block = take blockSize xs
    matches = anySame (map snd block)

anySame :: Eq a => [a] -> Bool
anySame = f []
  where
      f seen (x:xs) = x `elem` seen || f (x:seen) xs
      f seen [] = False

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:






run = do
  let file = "input.txt"
  contents <- readFile file
  print $ tune contents