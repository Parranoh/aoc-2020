import Data.List (foldl',sort)

main = interact $ show . solve . splitOn "" . lines

data Rule = Rule String (Integer -> Bool)
type Ticket = [Integer]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where
        go [] acc = [reverse acc]
        go (y:ys) acc =
            if x == y
            then reverse acc : go ys []
            else go ys (y:acc)

untilNoChange :: (Eq a) => (a -> a) -> a -> a
untilNoChange f = go
    where go x = let fx = f x in if fx == x then x else go fx

readRule :: String -> Rule
readRule s =
    let (n,s') = span (/= ':') s
        [_,a,_,b] = words s'
        [l1,u1] = map read . splitOn '-' $ a
        [l2,u2] = map read . splitOn '-' $ b
        v x = l1 <= x && x <= u1 || l2 <= x && x <= u2
    in Rule n v

readTicket :: String -> Ticket
readTicket = map read . splitOn ','

solve :: [[String]] -> Integer
solve [rs,[_,my],_:nearby] =
    let rules = map readRule $ rs
        ts = pruneInvalid . map readTicket $ nearby
        pruneInvalid = filter $ \t -> and [ or [ valid x r | r <- rules ] | x <- t ]
        possibles = (map . map) (\(Rule n _) -> n) $ foldl' f (repeat rules) ts
        ass = assign possibles
        indices = map fst . filter (departure . snd) . zip [0..] $ ass
    in product . map (readTicket my !!) $ indices
    where
        f :: [[Rule]] -> Ticket -> [[Rule]]
        f = zipWith . flip $ filter . valid

valid :: Integer -> Rule -> Bool
valid n (Rule _ v) = v n

departure :: String -> Bool
departure ('d':'e':'p':_) = True
departure _               = False

assign :: (Eq a) => [[a]] -> [a]
assign = map head . untilNoChange prune
    where
        prune ass =
            let known = [ a | [a] <- ass ]
                removeKnown as = case as of
                    as@[_] -> as
                    as     -> filter (`notElem` known) as
            in map removeKnown ass
