cycledeez = sum . go 1 1 . map parseLine . lines

data Command = NOOP | AddX Int deriving (Eq)

parseLine :: String -> Command
parseLine s
  | head s == 'n' = NOOP
  | otherwise = AddX $ read $ words s !! 1

cycles NOOP = 1
cycles (AddX _) = 2

isAddx (AddX _) = True
isAddx (_) = False

places = [20, 60, 100, 140, 180, 220]

go c x [] = []
go c x (comm : xs)
  | comm == NOOP = c + 1 `elem` places ? (x * (c + 1)) : go (c + 1) x xs ?: go (c + 1) x xs
  | isAddx comm = c + 1 `elem` places ? (x * (c + 1)) : go (c + 2) (x + val) xs ?: (c + 2 `elem` places ? ((x + val) * (c + 2)) : go (c + 2) (x + val) xs ?: go (c + 2) (x + val) xs)
  where
    (AddX val) = comm

go2 :: Int -> Int -> [Command] -> [Char]
go2 c x [] = []
go2 c x (comm : xs)
  | comm == NOOP = symbolNoop : go2 (c + 1) x xs
  | isAddx comm = symbolNoop : symbolAddx : go2 (c + 2) (x + val) xs
  where
    symbolNoop = abs (mod (c - 1) 40 - x) <= 1 ? '#' ?: '.'
    symbolAddx = abs (mod (c) 40 - x) <= 1 ? '#' ?: '.'
    (AddX val) = comm

cycledeez2 = go2 1 1 . map parseLine . lines

run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  let output = cycledeez2 contents
  print (take 40 output)
  print (take 40 . drop 40 $ output)
  print (take 40 . drop 80 $ output)
  print (take 40 . drop 120 $ output)
  print (take 40 . drop 160 $ output)
  print (drop 200 output)

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?: