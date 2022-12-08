import Data.Maybe
import Data.List

data Dir = Folder String [Dir] | File String Int deriving (Show)
data Command = Cd String | Ls deriving (Show, Eq) 




boom :: String -> Int
boom = findFinal . move (Folder "/" []) . lines 


move :: Dir -> [String] -> Dir
move d [] = d
move d@(Folder s dirs) (x:xs) 
  | isLs mc = doLs d xs
  | isCd mc && cd == ".." = d
  | isCd mc = move (Folder s (map shouldMove dirs)) (skipAhead xs 1)
  where
      Cd cd = fromMaybe (Cd "") mc
      (mc, md) = parseLine (x)
      shouldMove f@(Folder s _) = s == cd ? move f xs ?: f 
      shouldMove f = f

isFolder (Folder _ _) = True
isFolder _ = False

skipAhead [] _ = []
skipAhead xs 0 = xs
skipAhead (x:xs) c 
  | isCd mc && cd == ".." = skipAhead xs (c - 1)
  | isCd mc = skipAhead xs (c + 1)
  | otherwise = skipAhead xs c
  
  where
    Cd cd = fromMaybe (Cd "") mc
    (mc, md) = parseLine (x)

isCd (Just (Cd s)) = True
isCd _ = False
isLs (Just Ls) = True
isLs _ = False

doLs f [] = move f []
doLs f@(Folder s dirs) (x:xs) = isJust md ? doLs (Folder s (fromMaybe (Folder "" []) md : dirs)) xs ?: move f (x:xs)
  where
    (mc, md) = parseLine (x)
  

parseLine :: String -> (Maybe Command, Maybe Dir )
parseLine s 
  | head s == '$' && take 2 (drop 2 s) == "cd" = (Just (Cd (drop 5 s)), Nothing)
  | head s == '$' && take 2 (drop 2 s) == "ls" = (Just Ls, Nothing)
  | take 3 s == "dir" = (Nothing, Just (Folder (drop 4 s) []))
  | otherwise = (Nothing, Just (File (words s !! 1) (read $ words s !! 0)))


findFinal = sum . filter (<= 100000) . findSums

findSum :: Dir -> Int
findSum (File _ s) = s
findSum f@(Folder _ dirs) = (foldr ((+). findSum) 0 $ findFiles f) 

findSums :: Dir -> [Int]
findSums f@(Folder _ dirs) = (map findSum (findFolders f))


findFiles :: Dir -> [Dir]
findFiles f@(File _ _) = [f]
findFiles (Folder _ dirs) = concat $ map (findFiles) dirs

findFolders :: Dir -> [Dir]
findFolders f@(File _ _) = []
findFolders f@(Folder _ dirs) = f : (concat $ map (findFolders) dirs)

(?:) :: a -> a -> (a, a)
(?:) = (,)

(?) :: Bool -> (a, a) -> a
i ? (t, e) = if i then t else e

infix 0 ?

infix 1 ?:

--Failed to find a HLS version for GHC 9.2.5 Executable names we failed to find: haskell-language-server-9.2.5.exe,haskell-language-server.exe



run = do
  let file = "input.txt"
  contents <- readFile file
  print $ boom2 contents

boom2 :: String -> Int
boom2 = findBest . move (Folder "/" []) . lines 

findBest :: Dir -> Int
findBest ds = last $ filter (\a -> need <= (total - largest) + a ) sizes
  where
    largest = head sizes
    sizes = reverse $ sort $ findSums ds

total = 70000000
need = 30000000