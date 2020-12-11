import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
(...) = (.).(.)

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
        neighbors i = length . filter id . map (head . catMaybes) .
            (map . map) (a !#) . map ($ i) $ [n,e,s,w,ne,se,sw,nw]
        rule i occ
            | neighbors i == 0 = True
            | neighbors i >= 5 = False
            | otherwise        = occ
        n,e,s,w,ne,se,sw,nw :: (Int,Int) -> [(Int,Int)]
        n  = tail ... iterate $ bimap pred id
        e  = tail ... iterate $ bimap id   succ
        s  = tail ... iterate $ bimap succ id
        w  = tail ... iterate $ bimap id   pred
        ne = tail ... iterate $ bimap pred succ
        se = tail ... iterate $ bimap succ succ
        sw = tail ... iterate $ bimap succ pred
        nw = tail ... iterate $ bimap pred pred

(!#) :: Area -> (Int,Int) -> Maybe Bool
a !# i
    | IA.inRange (IA.bounds a) i = a ! i
    | otherwise                  = Just False

whileChange :: (Eq a) => (a -> a) -> a -> a
whileChange f x
    | x == f x  = x
    | otherwise = whileChange f $ f x

solve :: Area -> Int
solve = length . filter id . catMaybes . IA.elems . whileChange step

main = interact $ show . solve . parseArea
