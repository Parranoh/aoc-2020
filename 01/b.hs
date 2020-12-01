main = interact $ output . solve . input

input :: String -> [Int]
input = map read . lines

output :: Int -> String
output = show

solve :: [Int] -> Int
solve xs = head [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]
