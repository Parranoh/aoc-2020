import Data.List (foldl')
compose = foldr (.) id

type Position = (Integer,Integer,Integer,Integer) -- x,y,waypoint x,waypoint y

main = interact $ output . foldl' move (0,0,10,1) . lines

output :: Position -> String
output (x,y,_,_) = show $ abs x + abs y

move :: Position -> String -> Position
move pos@(x,y,wx,wy) (dir:n) = x `seq` y `seq` wx `seq` wy `seq` case dir of
    'N' -> (x,y,wx,wy + amount)
    'S' -> (x,y,wx,wy - amount)
    'E' -> (x,y,wx + amount,wy)
    'W' -> (x,y,wx - amount,wy)
    'L' -> compose (replicate (fromIntegral amount `div` 90) (\(x,y,wx,wy) -> (x,y,-wy,wx))) pos
    'R' -> compose (replicate (fromIntegral amount `div` 90) (\(x,y,wx,wy) -> (x,y,wy,-wx))) pos
    'F' -> (x + amount * wx,y + amount * wy,wx,wy)
    where
        amount = read n
