main = interact $ output . solve . input

solve :: [[Char]] -> Int
solve map = go 3 . tail $ map
    where
        width :: Int
        width = length . head $ map

        go :: Int -> [[Char]] -> Int
        go x (row:map)
            | row !! x == '#' = 1 + go ((x + 3) `mod` width) map
            | row !! x == '.' = go ((x + 3) `mod` width) map
        go _ _ = 0

input :: String -> [[Char]]
input = lines

output :: Int -> String
output = show
