
--ghci -XPatternSynonyms -XViewPatterns .\day12.hs
import qualified Data.Sequence as Seq hiding (zip, length)
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) 
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) 

data Node = Node {pos :: (Int,Int), parent :: Maybe Node, val :: Char, explored :: Bool} deriving (Show)

question1 :: String -> Int
question1 = stepCount (0,20) (139,20) . parse
  where
    parse = map (\(y, r) -> map (\(x, c) -> Node (x,y) Nothing c False) (zip [0..] r)) . zip [0..] . lines

bfs' :: Node -> Node -> [[Node]] -> Node
bfs' (Node ps p c _) = bfs (Seq.singleton (Node ps p c True)) (Node ps p c True)

bfs :: Seq.Seq Node -> Node -> Node -> [[Node]] -> Node
bfs Empty s e g = s
bfs (rest :> x) s e g 
  | found = x
  | otherwise = bfs newRest s e newG
  where
    newG = foldr replaceNode g newNs
    newRest = Seq.fromList newNs Seq.>< rest
    found = x == e
    newNs = map (\(Node ps _ c _) -> (Node ps (Just x) c True)) . filter (\(Node _ _ _ e) -> not e) $ ns
    ns = neighbours g x 

replaceNode :: Node -> [[Node]] -> [[Node]]
replaceNode n@(Node (x,y) _ _ _) grid = top ++ newRow ++ bottom
  where 
    top = take y grid
    bottom = drop (y + 1) grid
    newRow = [(take x . head . drop y $ grid) ++ (n : (drop (x + 1) . head . drop y $ grid))]

height :: Char -> Int
height c = c == 'S' ? indexOf 'a' alpha ?: (c == 'E' ? indexOf 'z' alpha ?: indexOf c alpha)
  where
    alpha = "abcdefghijklmnopqrstuvwxyz"

neighbours :: [[Node]] -> Node -> [Node]
neighbours grid (Node (sx,sy) _ c _) =  leftRight ++ upDown 
  where
    leftRight = [grid !! sy !! (sx + dx) |dx <- [-1,1], sx + dx >= 0 && sx + dx < length (head grid),  height (val (grid !! sy !! (sx + dx))) - currentHeight <=1]
    upDown = [ grid !! (sy + dy) !! sx  |dy <- [-1,1], sy + dy >= 0 && sy + dy < length grid ,  height (val (grid !! (sy + dy) !! sx)) - currentHeight <= 1]
    currentHeight = height c

trackBack :: Node -> [Node]
trackBack n@(Node _ (Nothing) _ _ ) = [n]
trackBack n@(Node _ (Just p) _ _ ) = n : trackBack p

indexOf :: Eq a => a -> [a] -> Int
indexOf = aux 0
  where
    aux _ _ [] = -1
    aux i a (x : xs) = a == x ? i ?: aux (i + 1) a xs

run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  let output = question2 contents
  print output

instance Eq Node where
  (==) (Node (ax,ay) _ _ _) (Node (bx,by) _ _ _) = ax == bx && ay == by

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:


stepCount (sx,sy) (ex,ey) g = (subtract 1) . length . trackBack . (\m -> bfs' (m !! sy !! sx) (m !! ey !! ex) m) $ g

stepCount' g (ex,ey) (sx,sy) = (subtract 1) . length . trackBack . (\m -> bfs' (m !! sy !! sx) (m !! ey !! ex) m) $ g

question2 :: String -> Int
question2 s = foldr (min . stepCount' grid (139,20)) 1000 candidates
  where
    candidates = concat . map (\(y,r) -> map (\(x, _) -> (x,y)) . filter (\(x,node) -> x < 3 && (val node == 'a' || val node == 'S')) $ r) $ zip [0..] (map (zip [0..]) grid)
    grid = map (\(y, r) -> map (\(x, c) -> Node (x,y) Nothing c False) (zip [0..] r)) . zip [0..] . lines $ s
