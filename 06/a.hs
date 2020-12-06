import Data.List (nub)

main = interact $ output . map solve . splitIntoParagraphs

splitIntoParagraphs :: String -> [String]
splitIntoParagraphs = go ""
    where
        go :: String -> String -> [String]
        go p "" = [reverse p]
        go p ('\n':'\n':s') = reverse p : splitIntoParagraphs s'
        go p (c:s') = go (c:p) s'

isLower :: Char -> Bool
isLower = flip elem ['a'..'z']

solve :: String -> Int
solve = length . nub . filter isLower

output :: [Int] -> String
output = show . sum
