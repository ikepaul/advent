import Data.Maybe (fromMaybe, isNothing)
import Prelude hiding (id)

data Monkey = Monkey {id :: Int, items :: [Int], operation :: Int -> Int, testNum :: Int, ifTrue :: Int, ifFalse :: Int, inspections :: Int}

instance Show Monkey where
  show m = show (id m) ++ " " ++ show (items m) ++ " " ++ show (testNum m) ++ " " ++ show (ifTrue m) ++ " " ++ show (ifFalse m) ++ " " ++ show (inspections m)

monkeyBusiness :: String -> Int
monkeyBusiness = uncurry (*) . foldr (\m (a, b) -> (max (inspections m) a, max (min (inspections m) a) b)) (0, 0) . (!! 10000) . iterate startRound . map parseMonkey . chunkList 7 . lines

startRound :: [Monkey] -> [Monkey]
startRound ms = newMs
  where
    newMs = map (giveItems over) finished
    (over, finished) = playRound [] ms

giveItems :: [(Int, Int)] -> Monkey -> Monkey
giveItems new (Monkey id' items' op test ifT ifF insp) = Monkey id' added op test ifT ifF insp
  where
    added = (items' ++) . map snd . filter ((id' ==) . fst) $ new

playRound :: [(Int, Int)] -> [Monkey] -> ([(Int, Int)], [Monkey])
playRound moved [] = (moved, [])
playRound moved allms@((Monkey id' items' op test ifT ifF insp) : ms) = (over, Monkey id' [] op test ifT ifF newInsp : nextRound)
  where
    newInsp = insp + length new
    (over, nextRound) = playRound (map (ifF,) throwFalse ++ map (ifT,) throwTrue ++ filter ((id' /=) . fst) moved) ms
    (throwTrue, throwFalse) = foldr (\n (t, f) -> n `mod` test == 0 ? (n : t, f) ?: (t, n : f)) ([], []) new
    -- new = map (flip div 3 . op) added
    new = map (flip mod 9699690 . op) added
    added = (items' ++) . map snd . filter ((id' ==) . fst) $ moved

parseMonkey :: [String] -> Monkey
parseMonkey ls = Monkey id' items' operation' testNum' ifTrue' ifFalse' 0
  where
    id' = read . take 1 . last . words . (!! 0) $ ls
    items' = map (read . take 2) . drop 2 . words . (!! 1) $ ls
    operation' = readExpression . drop 3 . words . (!! 2) $ ls
    testNum' = read . last . words . (!! 3) $ ls
    ifTrue' = read . last . words . (!! 4) $ ls
    ifFalse' = read . last . words . (!! 5) $ ls

readExpression :: [String] -> (Int -> Int)
readExpression s x = op (isNothing a ? x ?: fromMaybe 0 a) (isNothing b ? x ?: fromMaybe 0 b)
  where
    a = s !! 0 == "old" ? Nothing ?: Just (read $ s !! 0)
    op = s !! 1 == "+" ? (+) ?: (*)
    b = s !! 2 == "old" ? Nothing ?: Just (read $ s !! 2 :: Int)

run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  let output = monkeyBusiness $ contents
  print output

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as, bs) = splitAt n xs