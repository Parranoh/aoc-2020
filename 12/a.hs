import Data.List (foldl')

type Position = (Integer,Integer,Integer) -- x,y,angle

main = interact $ output . foldl' move (0,0,0) . lines

output :: Position -> String
output (x,y,_) = show $ abs x + abs y

move :: Position -> String -> Position
move (x,y,a) (dir:n) = x `seq` y `seq` a `seq` case dir of
    'N' -> (x,y + amount,a)
    'S' -> (x,y - amount,a)
    'E' -> (x + amount,y,a)
    'W' -> (x - amount,y,a)
    'L' -> (x,y,(a + amount) `mod` 360)
    'R' -> (x,y,(a - amount) `mod` 360)
    'F' -> case a of
        0   -> (x + amount,y,a)
        90  -> (x,y + amount,a)
        180 -> (x - amount,y,a)
        270 -> (x,y - amount,a)
    where
        amount = read n
