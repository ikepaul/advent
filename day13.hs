import Data.Char
import Data.List
import Data.Maybe

doit :: String -> Int
doit = foldr (\(i, (l, r)) acc -> fromMaybe True (correct (list . head $ l) (list . head $ r)) ? acc + i ?: acc) 0 . zip [1 ..] . map (\(l : r : _) -> (parseSignal l, parseSignal r)) . chunkList 3 . lines

parseSignal :: String -> [Mixed]
parseSignal [] = []
parseSignal (c : s)
  | c == ',' = parseSignal s
  | c == '[' = [List (parseSignal toNext)] ++ parseSignal (drop (length toNext + 1) s)
  | isNumber c && not (null s) && isNumber (head s) = Value 10 : parseSignal (tail s)
  | otherwise = Value (digitToInt c) : parseSignal s
  where
    toNext = (takeIt s 1)

takeIt _ 0 = []
takeIt ('[' : s) n = '[' : takeIt s (n + 1)
takeIt (']' : s) n = n - 1 == 0 ? [] ?: ']' : takeIt s (n - 1)
takeIt (c : s) n = c : takeIt s n

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = as : chunkList n bs where (as, bs) = splitAt n xs

data Mixed = List [Mixed] | Value Int deriving (Show, Eq)

instance Ord Mixed where
  compare a b = fromMaybe True (correct [a] [b]) ? LT ?: GT
  (<) a b = fromMaybe True (correct [a] [b])
  (<=) a b = fromMaybe True (correct [a] [b])
  (>) b a = fromMaybe True (correct [a] [b])
  (>=) b a = fromMaybe True (correct [a] [b])
  max b a = fromMaybe True (correct [a] [b]) ? a ?: b
  min a b = fromMaybe True (correct [a] [b]) ? a ?: b

isValue :: Mixed -> Bool
isValue (Value _) = True
isValue _ = False

val (Value x) = x

list (List x) = x

correct :: [Mixed] -> [Mixed] -> Maybe Bool
correct [] [] = Nothing
correct [] _ = Just True
correct _ [] = Just False
correct (l : ls) (r : rs)
  | isValue l && not (isValue r) = correct (List [l] : ls) (r : rs)
  | not (isValue l) && isValue r = correct (l : ls) (List [r] : rs)
  | isValue l && isValue r = val l == val r ? correct ls rs ?: (val l < val r ? Just True ?: Just False)
  | otherwise = isNothing (correct (list l) (list r)) ? correct ls rs ?: correct (list l) (list r)

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

run :: IO ()
run = do
  let file = "input.txt"
  contents <- readFile file
  let output = doitagain contents
  print output

doitagain :: String -> Int
doitagain = (\x -> (1 + indexOf (List [Value 2]) x) * (1 + indexOf (List [Value 6]) x)) . order . (List [Value 2] :) . (List [Value 6] :) . map (head . parseSignal) . filter ("" /=) . lines

indexOf :: Eq a => a -> [a] -> Int
indexOf = aux 0
  where
    aux _ _ [] = -1
    aux i a (x : xs) = a == x ? i ?: aux (i + 1) a xs

order :: [Mixed] -> [Mixed]
order = sort