import qualified Data.Set as S

main = interact $ output . solve . input

input :: String -> [Int]
input = map read . lines

output :: Int -> String
output = show

solve :: [Int] -> Int
solve = go S.empty
    where
        go _ [] = error "No match found"
        go s (x:xs)
            | (2020 - x) `S.member` s = x * (2020 - x)
            | otherwise               = go (S.insert x s) xs
