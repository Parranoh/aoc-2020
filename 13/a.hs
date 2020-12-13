main = interact $ show . uncurry (*) . solve . lines

commaToSpace :: Char -> Char
commaToSpace ',' = ' '
commaToSpace c   = c

solve :: [String] -> (Integer,Integer)
solve [n,ns] = minimum . map (\id -> (negate time `mod` id,id)) $ ids
    where
        time = read n
        ids = map read . filter (/= "x") . words . map commaToSpace $ ns
