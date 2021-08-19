import Data.List (sort,group,nub)
import qualified Data.Set as S

data Pos = Pos Int Int deriving (Eq,Ord,Show)
type Floor = S.Set Pos
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
solve = length . (!! 100) . iterate step . initial

initial :: [Tile] -> Floor
initial = S.fromList . map head . filter (odd . length) . group . sort . map tile2pos

tile2pos :: Tile -> Pos
tile2pos = mconcat . map dir2pos

dir2pos :: Dir -> Pos
dir2pos E  = Pos 1 0
dir2pos W  = Pos (-1) 0
dir2pos NE = Pos 0 1
dir2pos NW = Pos (-1) 1
dir2pos SE = Pos 1 (-1)
dir2pos SW = Pos 0 (-1)

step :: Floor -> Floor
step ps =
    let candidates = S.unions $ ps:(map neighbors $ S.toList ps)
    in S.filter white candidates
        where
            neighbors p = S.fromList [ p <> s | s <- map dir2pos [E,W,NE,NW,SE,SW] ]
            white p =
                let numBlackAdj = S.size $ neighbors p `S.intersection` ps
                in p `S.member` ps && numBlackAdj `elem` [1,2] ||
                    p `S.notMember` ps && numBlackAdj == 2
