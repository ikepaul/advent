import Data.List
import Data.Maybe

toAbyss = sendSands . getRocks

getRocks = concat . map parseLine . lines

sendSands :: [(Int, Int)] -> Int
sendSands rocks = isNothing ms ? 0 ?: 1 + sendSands (fromMaybe (0, 0) ms : rocks)
  where
    ms = sendSand rocks (500, 0)

sendSand :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
sendSand rocks (x, y) = isNothing newPos ? Just (x, y) ?: (y > maxY ? Nothing ?: sendSand rocks (fromMaybe (0, 0) newPos))
  where
    maxY = foldr (max . snd) 0 rocks
    newPos = (x, y + 1) `elem` rocks ? ((x - 1, y + 1) `elem` rocks ? ((x + 1, y + 1) `elem` rocks ? Nothing ?: Just (x + 1, y + 1)) ?: Just (x - 1, y + 1)) ?: Just (x, y + 1)

parseLine :: String -> [(Int, Int)]
parseLine = concat . map pairToCords . pairs . filter (/= "->") . words

pairToCords :: (String, String) -> [(Int, Int)]
pairToCords (a, b) = [(x, y) | x <- [(min ax bx) .. (max ax bx)], y <- [(min ay by) .. (max ay by)]]
  where
    (ax, ay) = (read . takeWhile (/= ',') $ a, read . tail . dropWhile (/= ',') $ a)
    (bx, by) = (read . takeWhile (/= ',') $ b, read . tail . dropWhile (/= ',') $ b)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

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
  let output = toAbyss2 $ contents
  print output

toAbyss2 = sendSands2 161. foldr replacePos (replicate 200 (replicate 1000 False)) . getRocks

replacePos :: (Int,Int) -> [[Bool]] -> [[Bool]]
replacePos (x,y) grid = top ++ newRow ++ bottom
  where 
    top = take y grid
    bottom = drop (y + 1) grid
    newRow = [(take x . head . drop y $ grid) ++ (True : (drop (x + 1) . head . drop y $ grid))]


sendSands2 :: Int -> [[Bool]] -> Int
sendSands2 floor rocks = isNothing ms ? 0 ?: 1 + sendSands2 floor (replacePos (fromMaybe (0, 0) ms)  rocks)
  where
    ms = sendSand2 floor rocks (500, 0)

sendSand2 :: Int -> [[Bool]] -> (Int, Int) -> Maybe (Int, Int)
sendSand2 floor rocks (x, y) = isNothing newPos ? (y == 0 ? Nothing ?: Just (x, y)) ?: sendSand2 floor rocks (fromMaybe (0, 0) newPos)
  where
    newPos = getPos floor rocks (x, y)
 
getPos :: Int -> [[Bool]]-> (Int, Int) -> Maybe (Int, Int)
getPos floor rocks (x, y) = y == floor - 1 ? Nothing ?: (rocks !! (y+1) !! x ? (rocks !! (y+1) !! (x-1) ? (rocks !! (y+1) !! (x+1) ? Nothing ?: Just (x + 1, y + 1)) ?: Just (x - 1, y + 1)) ?: Just (x, y + 1))
