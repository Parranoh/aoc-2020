import Data.List (sort,group)

data Pos = Pos Int Int deriving (Eq,Ord)
type Floor = [Pos]
data Dir = E | W | NE | SE | NW | SW deriving (Show)
type Tile = [Dir]

instance Semigroup Pos where
    Pos x y <> Pos x' y' = Pos (x + x') (y + y')

instance Monoid Pos where
    mempty = Pos 0 0

main = interact $ show . solve . input

input :: String -> [Tile]
input = map parseTile . lines
    where
        parseTile "" = []
        parseTile ('e':s) = E : parseTile s
        parseTile ('w':s) = W : parseTile s
        parseTile ('n':'e':s) = NE : parseTile s
        parseTile ('n':'w':s) = NW : parseTile s
        parseTile ('s':'e':s) = SE : parseTile s
        parseTile ('s':'w':s) = SW : parseTile s

solve :: [Tile] -> Int
solve = length . filter (odd . length) . group . sort . map tile2pos

tile2pos :: Tile -> Pos
tile2pos = mconcat . map dir2pos
    where
        dir2pos E  = Pos 1 0
        dir2pos W  = Pos (-1) 0
        dir2pos NE = Pos 0 1
        dir2pos NW = Pos (-1) 1
        dir2pos SE = Pos 1 (-1)
        dir2pos SW = Pos 0 (-1)
