main = interact $ output . map (isValidPass . passToMap) . splitIntoPass

splitIntoPass :: String -> [String]
splitIntoPass "" = []
splitIntoPass s  = go "" s
    where
        go :: String -> String -> [String]
        go p "" = [reverse p]
        go p ('\n':'\n':s') = reverse p : splitIntoPass s'
        go p (c:s') = go (c:p) s'

isValidPass :: [(String,String)] -> Bool
isValidPass = and . flip map tests . flip ($)

tests :: [[(String,String)] -> Bool]
tests = [byr,iyr,eyr,hgt,hcl,ecl,pid]

test :: String -> (String -> Bool) -> [(String,String)] -> Bool
test key t m = case lookup key m of
    Nothing -> False
    Just v  -> t v

isDigit :: Char -> Bool
isDigit = flip elem ['0'..'9']

byr = test "byr" $ \y -> "1920" <= y && y <= "2002" && length y == 4 && all isDigit y
iyr = test "iyr" $ \y -> "2010" <= y && y <= "2020" && length y == 4 && all isDigit y
eyr = test "eyr" $ \y -> "2020" <= y && y <= "2030" && length y == 4 && all isDigit y
hgt = test "hgt" $ \h -> case h of
    '1':a:b:"cm" -> "50" <= [a,b] && [a,b] <= "93" && all isDigit [a,b]
    a:b:"in"     -> "59" <= [a,b] && [a,b] <= "76" && all isDigit [a,b]
    _            -> False
hcl = test "hcl" $ \c -> case c of
    '#':c' -> length c' == 6 && all (`elem` ['a'..'f'] ++ ['0'..'9']) c'
    _      -> False
ecl = test "ecl" $ flip elem ["amb","blu","brn","gry","grn","hzl","oth"]
pid = test "pid" $ \p -> length p == 9 && all isDigit p

passToMap :: String -> [(String,String)]
passToMap = (map . fmap . drop $ 1) . map (span (/= ':')) . words

output :: [Bool] -> String
output = show . length . filter id
