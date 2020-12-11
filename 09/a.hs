main = interact $ show . uncurry solve . splitAt 25 . map read . lines

solve :: [Integer] -> [Integer] -> Integer
solve preamble (x:xs)
    | null [() | a <- preamble, b <- preamble, a /= b, a + b == x] = x
    | otherwise = solve (tail preamble ++ [x]) xs
