main = interact $ output . solve . input

output :: Int -> String
output = show

input :: String -> [(Int, Int, Char, String)]
input = map parseLine . lines

parseLine l =
    let (lower,'-':l') = span (/= '-') l
        (upper,' ':l'') = span (/= ' ') l'
        ([char], ':':' ':password) = span (/= ':') l''
    in (read lower, read upper, char, password)

solve :: [(Int, Int, Char, String)] -> Int
solve = length . filter id . map solveLine

solveLine :: (Int, Int, Char, String) -> Bool
solveLine (l,u,c,p) = l <= occ && occ <= u
    where
        occ = length . filter (== c) $ p
