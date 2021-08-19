type State = ([Integer],[Integer])

main = interact $ show . solve . input

input :: String -> State
input s =
    let (_:p1,_:_:p2) = break null $ lines s
    in (map read p1,map read p2)

solve :: State -> Integer
solve (xs,[]) = score xs
solve ([],ys) = score ys
solve st      = solve . step $ st

step :: State -> State
step (x:xs,y:ys)
    | x > y     = (xs ++ [x,y],ys)
    | otherwise = (xs,ys ++ [y,x])

score :: [Integer] -> Integer
score = snd . foldr (\x (i,accum) -> (succ i,i * x + accum)) (1,0)
