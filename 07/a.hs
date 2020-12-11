import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Data.Map.Strict as M
import Data.Set ((\\))
import qualified Data.Set as S

data Color = Color String String deriving (Eq,Ord)

data Graph a = Graph (M.Map a [(Integer,a)])

main = interact $ show . subtract 1 . either (error . show) solve . P.parse rules "mulm"

myColor :: Color
myColor = Color "shiny" "gold"

rules :: P.Parsec String () [(Color,[(Integer,Color)])]
rules = rule `P.endBy` P.char '\n'
rule = do
    c <- color
    P.string " bags contain "
    cs <- colorList
    P.char '.'
    return (c,cs)
colorList = P.try (P.string "no other bags" >> return [])
    <|> colorCount `P.sepBy` P.string ", "
colorCount = do
    count <- (read :: String -> Integer) <$> P.many1 P.digit
    P.skipMany1 P.space
    col <- color
    P.skipMany1 P.space
    P.string "bag" >> P.optional (P.char 's')
    return (count,col)
color = do
    mod <- word
    P.skipMany1 P.space
    col <- word
    return $ Color mod col
word = P.many1 P.letter

solve :: [(Color,[(Integer,Color)])] -> Int
solve = length . bfs myColor . transform

transform :: (Ord a) => [(a,[(Integer,a)])] -> Graph a
transform xs = Graph (M.fromListWith (++) $ concatMap f xs)
    where
        f (v,es) = map (\(w,v') -> (v',[(w,v)])) es

bfs :: (Ord a) => a -> Graph a -> S.Set a
bfs s g = go (S.singleton s) S.empty
    where
        go work done
            | S.null work = done
            | otherwise   = go (S.fromList (concatMap (flip neighbors g) work) \\ work \\ done) (work `S.union` done)

neighbors :: (Ord a) => a -> Graph a -> [a]
neighbors v (Graph es) = map snd . M.findWithDefault [] v $ es
