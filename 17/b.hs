import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA

                    -- w  z   y   x
type Map = IA.Array (Int,Int,Int,Int) Bool

main = interact $ show . solve . input

input :: String -> Map
input s = IA.listArray ((0,0,1,1),(0,0,length $ lines s,length . head $ lines s)) .
    map (== '#') .  filter (/= '\n') $ s

step :: Map -> Map
step m =
    let ((minW,minZ,minY,minX),(maxW,maxZ,maxY,maxX)) = IA.bounds m
        maxBounds = ((minW-1,minZ-1,minY-1,minX-1),(maxW+1,maxZ+1,maxY+1,maxX+1))
    in IA.array maxBounds [ (i,rule i m) | i <- IA.range maxBounds ]

rule :: (Int,Int,Int,Int) -> Map -> Bool
rule i@(w,z,y,x) m
    | neighbors == 3 = True
    | neighbors == 2 = m !? i
    | otherwise      = False
    where neighbors = length . filter id . map (m !?) . filter (/= i) $
            [ (w+d,z+c,y+b,x+a) | d <- [-1..1], c <- [-1..1], b <- [-1..1], a <- [-1..1] ]

solve :: Map -> Int
solve = length . filter id . IA.elems . (!! 6) . iterate step

(!?) :: Map -> (Int,Int,Int,Int) -> Bool
m !? i
    | IA.inRange (IA.bounds m) i = m ! i
    | otherwise            = False
