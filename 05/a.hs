import qualified Numeric as N

main = interact $ output . solve . input

input :: String -> [String]
input = lines

solve :: [String] -> Integer
solve = maximum . map seatToId

seatToId :: String -> Integer
seatToId = fst . head . N.readInt
    2
    (const True)
    (\c -> if c == 'B' || c == 'R' then 1 else 0)

output :: Integer -> String
output = show
