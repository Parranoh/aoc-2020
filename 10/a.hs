import Data.List (sort)

main = interact $ show . solve . sort . map read . lines

solve :: [Integer] -> Int
solve xs = (length $ filter (== 3) ds) * (length $ filter (== 1) ds)
    where
        ds = map (negate . uncurry (-)) . zip xs' . tail $ xs'
        xs' = 0 : xs ++ [last xs + 3]
