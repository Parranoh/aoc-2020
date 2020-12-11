import Data.List (sort)

main = interact $ show . solve . sort . map read . lines

solve :: [Integer] -> Integer
solve = go (0,0,1) 1
    where
        go (_,_,c) _ [] = c
        go (a,b,c) n xs@(x:xs')
            | n == x    = go (b,c,a + b + c) (succ n) xs'
            | otherwise = go (b,c,0) (succ n) xs
