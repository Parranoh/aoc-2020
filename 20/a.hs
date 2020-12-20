import Data.List (sort)
import Numeric (readInt)
import Data.Function
splitOn x xs = go xs []
    where
        go [] acc = [reverse acc]
        go (y:ys) acc =
            if x == y
            then reverse acc : go ys []
            else go ys (y:acc)

singles :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
singles (=#) = go . sort
    where
        go []                    = []
        go (x:xs@(y:_)) | x =# y = go . dropWhile (=# x) $ xs
        go (x:xs)                = x : go xs

doubles :: (Ord a) => [a] -> [a]
doubles = go . sort
    where
        go [] = []
        go (x:y:z:w:xs)
            | x == y && y == z && z == w = x : go xs
        go (x:xs) = go xs

edges :: [String] -> [Int]
edges tile =
    let top = head tile
        left = map head tile
        right = map last tile
        bottom = last tile
        es' = [top,left,right,bottom]
        es = es' ++ map reverse es'
    in map (fst . head . readInt 2 (const True) (fromEnum . (== '#'))) es

readTile :: [String] -> [(Int,Integer)]
readTile (n:tile) =
    let num = read . init . last . words $ n
    in [ (border,num) | border <- edges tile ]

corners :: [(Int,Integer)] -> [Integer]
corners = map snd . singles ((==) `on` fst)

solve :: String -> Integer
solve = product . doubles . corners . concatMap readTile .
    filter (not . null) . splitOn "" . lines

main = interact $ show . solve
