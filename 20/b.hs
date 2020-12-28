import Data.List (sort,unfoldr)
import Numeric (readInt)
import Data.Function
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
splitOn x xs = go xs []
    where
        go [] acc = [reverse acc]
        go (y:ys) acc =
            if x == y
            then reverse acc : go ys []
            else go ys (y:acc)

data Orientation = Orientation Rotation Bool deriving (Eq,Ord,Show)
data Rotation = N | E | S | W deriving (Eq,Show)
instance Ord Rotation where
    N <= E = True
    E <= S = True
    S <= W = True
    W <= N = True
    _ <= _ = False
mirror (Orientation r f) = Orientation r $ not f
opposite (Orientation N f) = Orientation S f
opposite (Orientation E f) = Orientation W f
opposite (Orientation S f) = Orientation N f
opposite (Orientation W f) = Orientation E f
type Edge = Int
type Id = Int
data Tile = Tile
    { ident :: Id
    , es :: [(Orientation,Edge)]
    , content :: [[Bool]] }

edges :: [String] -> [(Orientation,Edge)]
edges tile =
    let top = (Orientation N False,head tile)
        right = (Orientation E False,map last tile)
        bottom = (Orientation S False,last tile)
        left = (Orientation W False,map head tile)
        es' = [top,right,bottom,left]
    in concatMap (\(o,e) -> [(o,readBin e),(mirror o,readBin $ reverse e)]) es'

readBin :: String -> Edge
readBin = fst . head . readInt 2 (const True) (fromEnum . (== '#'))

readTile :: [String] -> Tile
readTile (n:tile) =
    let num = read . init . last . words $ n
        tile' = (map . map) (== '#') tile
        cropped = map inner . inner $ tile'
    in Tile num (edges tile) cropped
    where inner = init . tail

lookupTile :: Edge -> [Tile] -> Maybe (Orientation,Tile)
lookupTile _ [] = Nothing
lookupTile e (t@(Tile _ es _) : ts)
    | null es'  = lookupTile e ts
    | otherwise = Just (fst $ head es',t)
    where es' = filter ((== e) . snd) es

lookupId :: Id -> [Tile] -> Tile
lookupId i = head . filter (\(Tile i' _ _) -> i == i')

corner :: [Tile] -> Id
corner = single . sort . concatMap (\(Tile i es _) -> map (\(_,e) -> (e,i)) es)
    where
        single :: (Eq a) => [(a,b)] -> b
        single (x:y:xs)
            | fst x /= fst y = snd x
            | otherwise      = single xs

-- solve :: [Tile] -> Int
solve tiles =
    let tile = flip lookupId tiles
        topLeftId = corner tiles
        tlOri = {-minimum .-} filter (isNothing . flip lookupTile (filter (\t -> ident t /= topLeftId) tiles) . snd) . es . tile $ topLeftId
    in tlOri

input :: String -> [Tile]
input = map readTile . filter (not . null) . splitOn "" . lines

main = interact $ show . solve . input
