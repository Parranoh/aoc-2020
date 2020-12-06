import Data.List (nub)
import Control.Monad (guard)

main = interact $ output . map solve . splitIntoParagraphs

splitIntoParagraphs :: String -> [String]
splitIntoParagraphs = go ""
    where
        go :: String -> String -> [String]
        go p "" = [reverse p]
        go p ('\n':'\n':s') = reverse p : splitIntoParagraphs s'
        go p (c:s') = go (c:p) s'

solve :: String -> Int
solve = length . getAll . words

getAll :: [String] -> [Bool]
getAll = filter id . foldr (zipWith (&&)) (repeat True) . map getPassenger

getPassenger :: String -> [Bool]
getPassenger = flip map (map elem ['a'..'z']) . flip ($)

output :: [Int] -> String
output = show . sum
