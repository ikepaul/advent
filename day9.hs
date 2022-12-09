bridgerope :: String -> Int
bridgerope = length . move (0, 0) (0, 0) [(0, 0)] . concatMap parseLine . lines

parseLine :: String -> [Move]
parseLine s = replicate c m
  where
    m = stom $ head ws
    c = read $ ws !! 1
    ws = words s

stom s = case s of
  "R" -> RIGHT
  "U" -> UP
  "D" -> DOWN
  "L" -> LEFT

data Move = UP | DOWN | RIGHT | LEFT deriving (Eq, Show)

move :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [Move] -> [(Int, Int)]
move _ _ spots [] = spots
move (hx, hy) (tx, ty) spots (m : ms) = move nextH nextT newSpots ms
  where
    newSpots = nextT `elem` spots ? spots ?: (nextT : spots)
    nextT = changePos nextH (tx, ty)
    nextH = case m of
      UP -> (hx, hy + 1)
      DOWN -> (hx, hy - 1)
      RIGHT -> (hx + 1, hy)
      LEFT -> (hx - 1, hy)

changePos :: (Int, Int) -> (Int, Int) -> (Int, Int)
changePos (hx, hy) (tx, ty)
  | abs (hx - tx) >= 1 && abs (hy - ty) >= 2 || abs (hx - tx) >= 2 && abs (hy - ty) >= 1 = (tx + (hx - tx > 0 ? 1 ?: -1), ty + (hy - ty > 0 ? 1 ?: -1))
  | otherwise = (newX, newY)
  where
    newX = abs (hx - tx) >= 2 ? tx + (hx - tx > 0 ? 1 ?: -1) ?: tx
    newY = abs (hy - ty) >= 2 ? ty + (hy - ty > 0 ? 1 ?: -1) ?: ty


bridgerope2 :: String -> Int
bridgerope2 = length . move2 (replicate 10 (0,0)) [(0, 0)] . concatMap parseLine . lines

move2 :: [(Int, Int)] -> [(Int, Int)] -> [Move] -> [(Int, Int)]
move2 _ spots [] = spots
move2 ((hx,hy):ps) spots (m : ms) = move2 newPs newSpots ms
  where
    newSpots = (last newPs) `elem` spots ? spots ?: ((last newPs) : spots)
    newPs = nextH : moveThrough (nextH:ps)
    nextH = case m of
      UP -> (hx, hy + 1)
      DOWN -> (hx, hy - 1)
      RIGHT -> (hx + 1, hy)
      LEFT -> (hx - 1, hy)

moveThrough :: [(Int,Int)] -> [(Int,Int)]
moveThrough [x] = []
moveThrough (a: b :rs) = changePos a b : moveThrough (changePos a b : rs)















run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  print $ bridgerope2 contents

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?: