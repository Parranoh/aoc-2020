import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA

type Map = IA.Array (Int,Int,Int) Bool

main = interact $ show . solve . input

                            -- z  y   x
input :: String -> Map
input s = IA.listArray ((0,1,1),(0,length $ lines s,length . head $ lines s)) .
    map (== '#') .  filter (/= '\n') $ s

step :: Map -> Map
step m =
    let ((minZ,minY,minX),(maxZ,maxY,maxX)) = bounds m
        maxBounds = ((minZ - 1,minY - 1,minX - 1),(maxZ + 1,maxY + 1,maxX + 1))

rule i@(z,y,x) m =
    | neighbors == 3 = True
    | neighbors == 2 = m ! i
    | otherwise      = False
    where neighbors = length . filter id . map (m !?) . filter (/= i) $
            [ (z + c,y + b,x + a) | c <- [-1..1], b <- [-1..1], a <- [-1..1]) ]

solve = id

(!?) :: Map -> (Int,Int,Int) -> Bool
m !? i
    | IA.inRange (IA.bounds m) i = m ! i
    | otherwise            = False
