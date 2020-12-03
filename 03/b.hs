main = interact $ output . solve . input

solve :: [[Char]] -> Int
solve terrain = product . map (flip solveOne terrain) $ [(1,1),(3,1),(5,1),(7,1),(1,2)]

solveOne :: (Int,Int) -> [[Char]] -> Int
solveOne slope@(dx,dy) terrain = go dx . drop dy $ terrain
    where
        width :: Int
        width = length . head $ terrain

        go :: Int -> [[Char]] -> Int
        go x terrain'@(row:_)
            | row !! x == '#' = succ . go ((x + dx) `mod` width) . drop dy $ terrain'
            | row !! x == '.' = go ((x + dx) `mod` width) . drop dy $ terrain'
        go _ _ = 0

input :: String -> [[Char]]
input = lines

output :: Int -> String
output = show
