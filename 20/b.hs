import Data.List (sort,transpose)
import Numeric (readInt)
import Data.Function
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import Data.Array.Unboxed as IA
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
type Edge = Int
type Id = Int
data Tile = Tile
    { ident :: Id
    , edges :: [(Orientation,Edge)]
    , content :: [[Bool]] }
    deriving Show
type Sea = IA.UArray (Int,Int) Bool

rotate' :: Rotation -> Rotation
mirror, rotate, opposite :: Orientation -> Orientation
mirror (Orientation r f) = Orientation r $ not f
rotate' N = W
rotate' E = N
rotate' S = E
rotate' W = S
rotate (Orientation r False) = Orientation (rotate' r) False
rotate (Orientation r True ) = Orientation (rotate' . rotate' . rotate' $ r) True
opposite = mirror . rotate . rotate

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a,b) (c,d) = (a + c,b + d)

applyOrientationAndOffset :: Orientation -> (Int,Int) -> Tile -> [[((Int,Int),Bool)]]
applyOrientationAndOffset ori offset =
    (map . map . first) (add offset) .
    zipWith f [0..] .
    apply ori .
    content
    where
        f :: Int -> [Bool] -> [((Int,Int),Bool)]
        f y = zipWith (\x c -> ((y,x),c)) [0..]

        apply :: Orientation -> [[a]] -> [[a]]
        apply (Orientation W m)    = apply (Orientation N m) . transpose . reverse
        apply (Orientation S m)    = apply (Orientation N m) . map reverse . reverse
        apply (Orientation E m)    = apply (Orientation N m) . reverse . transpose
        apply (Orientation N True) = map reverse
        apply _                    = id

readEdges :: [String] -> [(Orientation,Edge)]
readEdges tile =
    let top = (Orientation N False,head tile)
        right = (Orientation E False,map last tile)
        bottom = (Orientation S True,last tile)
        left = (Orientation W True,map head tile)
        es' = [top,right,bottom,left]
    in concatMap (\(o,e) -> [(o,readBin e),(mirror o,readBin $ reverse e)]) es'

readBin :: String -> Edge
readBin = fst . head . readInt 2 (const True) (fromEnum . (== '#'))

readTile :: [String] -> Tile
readTile (n:tile) =
    let num = read . init . last . words $ n
        tile' = (map . map) (== '#') tile
        cropped = map inner . inner $ tile'
    in Tile num (readEdges tile) cropped -- TODO
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
corner = quad . sort . singles . sort . concatMap (\(Tile i es _) -> map (\(_,e) -> (e,i)) es)
    where
        singles :: (Eq a) => [(a,b)] -> [b]
        singles [] = []
        singles [x] = [snd x]
        singles (x:y:xs)
            | fst x /= fst y = snd x : singles (y:xs)
            | otherwise      = singles xs
        quad :: (Eq a) => [a] -> a
        quad (a:r@(b:c:d:_))
            | a == b && b == c && c == d = a
            | otherwise                  = quad r

allOrientations :: [((Int,Int),a)] -> [[((Int,Int),a)]]
allOrientations m =
    let n = m
        e = transpose $ m
        w = transpose . flip $ m
        s = transpose . flip $ w
        all = [n,e,w,s]
    in all ++ map flip all
    where
        transpose = map $ \((x,y),b) -> ((y,x),b)
        flip = map $ \((x,y),b) -> ((x,height - y),b)
        height = maximum . map (\((_,y),_) -> y) $ m

monster :: [(Int,Int)]
monster =
    [ (1, 0)
    , (2, 1)
    , (2, 4)
    , (1, 5)
    , (1, 6)
    , (2, 7)
    , (2,10)
    , (1,11)
    , (1,12)
    , (2,13)
    , (2,16)
    , (1,17)
    , (0,18)
    , (1,18)
    , (1,19) ]

(!?) :: Sea -> (Int,Int) -> Bool
s !? i
    | IA.inRange (IA.bounds s) i = s IA.! i
    | otherwise            = False

countMonsters :: Sea -> Int
countMonsters s = length . filter isMonster . IA.indices $ s
    where
        isMonster :: (Int,Int) -> Bool
        isMonster pos = and [ s !? i | offset <- monster, let i = add offset pos ]

solve :: [Tile] -> Int
solve tiles =
    let tile = flip lookupId tiles
        matching o i =
            lookupTile (
                maybe (error "edge not found") id .
                lookup (opposite o) .
                edges .
                tile $
                i) .
            filter (\t -> ident t /= i) $
            tiles
        topLeftId = corner tiles
        tlOri =
            mirror .
            fst .
            minimum .
            filter (
                isNothing .
                flip lookupTile (filter (\t -> ident t /= topLeftId) tiles) .
                snd) .
            edges .
            tile $
            topLeftId
        arrange :: (Orientation,Id) -> [(Orientation,Id)]
        arrange (o,i) = (o,i) : case matching o i of
            Nothing     -> []
            Just (o',t) -> arrange (o',ident t)
        arranged =
            (map . map . first) (rotate . rotate . rotate) .
            map (arrange . first rotate) .
            arrange $
            (tlOri,topLeftId)
        assocs = concat . (concatMap . concatMap) ((uncurry . uncurry) applyOrientationAndOffset .
            fmap tile) . zipWith f [0,8..] $ arranged
        pixels = length . filter id . map snd $ assocs
        dim = maximum . map (fst . fst) $ assocs
        maps = map (IA.array ((0,0),(dim,dim))) . allOrientations $ assocs :: [Sea]
    in pixels - length monster * maximum (map countMonsters maps)
    where
        f :: Int -> [(Orientation,Id)] -> [((Orientation,(Int,Int)),Id)]
        f y = zipWith (\x (o,i) -> ((o,(y,x)),i)) [0,8..]

input :: String -> [Tile]
input = map readTile . filter (not . null) . splitOn "" . lines

main = interact $ show . solve . input
