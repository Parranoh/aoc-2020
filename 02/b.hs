main = interact $ output . solve . input

output :: Int -> String
output = show

input :: String -> [(Int, Int, Char, String)]
input = map parseLine . lines

parseLine l =
    let (first,'-':l') = span (/= '-') l
        (second,' ':l'') = span (/= ' ') l'
        ([char], ':':' ':password) = span (/= ':') l''
    in (read first - 1, read second - 1, char, password)

solve :: [(Int, Int, Char, String)] -> Int
solve = length . filter id . map solveLine

solveLine :: (Int, Int, Char, String) -> Bool
solveLine (f,s,c,p) = (p !! f == c) /= (p !! s == c)
