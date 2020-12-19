main = interact $ show . solve . splitOn "" . lines

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where
        go [] acc = [reverse acc]
        go (y:ys) acc =
            if x == y
            then reverse acc : go ys []
            else go ys (y:acc)

solve :: [[String]] -> Integer
solve [rules,_:my,_:nearby] =
    sum .
    filter invalid .
    concatMap
        (map (read :: String -> Integer) .
        splitOn ',') $
    nearby
    where
        invalid :: Integer -> Bool
        invalid n =
            not .
            or .
            map ($ n) .
            map (\[l,u] x -> l <= x && x <= u) .
            concatMap
                (map
                    (map (read :: String -> Integer) .
                    splitOn '-') .
                filter ('-' `elem`) .
                words) $
            rules
