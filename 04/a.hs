main = interact $ output . map isValidPass . splitIntoPass

splitIntoPass :: String -> [String]
splitIntoPass "" = []
splitIntoPass s  = go "" s
    where
        go :: String -> String -> [String]
        go p "" = [reverse p]
        go p ('\n':'\n':s') = reverse p : splitIntoPass s'
        go p (c:s') = go (c:p) s'

isValidPass :: String -> Bool
isValidPass = and . flip map (map elem reqKeys) . flip ($) . passToKeys

passToKeys :: String -> [String]
passToKeys = map (takeWhile (/= ':')) . words

output :: [Bool] -> String
output = show . length . filter id

reqKeys :: [String]
reqKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
