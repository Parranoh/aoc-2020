import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA
import Data.Maybe (catMaybes)

type Area = IA.Array (Int,Int) (Maybe Bool)

parseSeat :: Char -> Maybe Bool
parseSeat '.' = Nothing
parseSeat 'L' = Just False

parseArea :: String -> Area
parseArea s = IA.listArray ((1,1),(h,w)) . map parseSeat . filter (/= '\n') $ s
    where
        h = length . lines $ s
        w = length . head . lines $ s

step :: Area -> Area
step a = IA.array (IA.bounds a) [ (i,fmap (rule i) occ) | (i, occ) <- IA.assocs a ]
    where
        neighbors (y,x) = length . filter id . catMaybes . map (a !#) $
            [(y,x-1),(y,x+1),(y-1,x),(y+1,x),(y-1,x-1),(y-1,x+1),(y+1,x-1),(y+1,x+1)]
        rule i occ
            | neighbors i == 0 = True
            | neighbors i >= 4 = False
            | otherwise        = occ

(!#) :: Area -> (Int,Int) -> Maybe Bool
a !# i
    | IA.inRange (IA.bounds a) i = a ! i
    | otherwise                  = Nothing

whileChange :: (Eq a) => (a -> a) -> a -> a
whileChange f x
    | x == f x  = x
    | otherwise = whileChange f $ f x

solve :: Area -> Int
solve = length . filter id . catMaybes . IA.elems . whileChange step

main = interact $ show . solve . parseArea
