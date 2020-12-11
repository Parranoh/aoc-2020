main = interact $ show . solve . map read . lines

solveA :: [Integer] -> [Integer] -> Integer
solveA preamble (x:xs)
    | null [() | a <- preamble, b <- preamble, a /= b, a + b == x] = x
    | otherwise = solveA (tail preamble ++ [x]) xs

solve :: [Integer] -> Integer
solve xs = (\xs -> maximum xs + minimum xs) . head . filter ((== goal) . sum) . sets $ xs
    where
        goal = uncurry solveA . splitAt 25 $ xs

sets :: [Integer] -> [[Integer]]
sets xs = go 2 xs
    where
        len = length xs
        go n xs'
            | length (take n xs') < n = if n == len then [] else go (succ n) xs
            | otherwise               = set : go n (tail xs')
            where set = take n xs'
