type State = (Int,[Int],Int,[Int])

main = interact $ show . solve . input

input :: String -> State
input s =
    let (_:p1,_:_:p2) = break null $ lines s
    in (length p1,map read p1,length p2,map read p2)

solve :: State -> Int
solve = score . either id id . game

game :: State -> Either [Int] [Int]
game st = go [st]
    where
        go ((_,xs,_,[]):_) = Left xs
        go ((_,[],_,ys):_) = Right ys
        go sts@(st:log)      =
            let st' = step log st
            in go (st':sts)

step :: [State] -> State -> State
step log st@(i,p1@(x:xs),j,y:ys)
    | st `elem` log = (i,p1,0,[])
    | i > x && j > y =
        case game (x,take x xs,y,take y ys) of
            Left  _ -> leftWin
            Right _ -> rightWin
    | x > y     = leftWin
    | otherwise = rightWin
    where
        leftWin  = (succ i,xs ++ [x,y],pred j,ys)
        rightWin = (pred i,xs,succ j,ys ++ [y,x])

score :: [Int] -> Int
score = snd . foldr (\x (i,accum) -> (succ i,i * x + accum)) (1,0)
