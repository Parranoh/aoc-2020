import qualified Numeric as N
import Data.List (sort)

main = interact $ output . solve . input

input :: String -> [String]
input = lines

solve :: [String] -> Integer
solve = findSeat . sort . map seatToId

findSeat :: [Integer] -> Integer
findSeat (a:r@(b:_))
    | a + 1 /= b = a + 1
    | otherwise  = findSeat r

seatToId :: String -> Integer
seatToId = fst . head . N.readInt
    2
    (const True)
    (\c -> if c == 'B' || c == 'R' then 1 else 0)

output :: Integer -> String
output = show
