import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Data.Map.Strict as M
import Data.Set ((\\))
import qualified Data.Set as S

data Color = Color String String deriving (Eq,Ord)

newtype Graph a = Graph (M.Map a [(Integer,a)])

data Tree a = Node a [(Integer,Tree a)]

treeFold :: (a -> [(Integer,b)] -> b) -> Tree a -> b
treeFold f (Node a cs) = f a (map (fmap $ treeFold f) cs)

main = interact $ show . either (error . show) solve . P.parse rules "mulm"

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

solve :: [(Color,[(Integer,Color)])] -> Integer
solve = treeFold f . dagToTree myColor . transform
    where
        f = const $ sum . map (uncurry (*) . fmap succ)

transform :: (Ord a) => [(a,[(Integer,a)])] -> Graph a
transform xs = Graph $ M.fromListWith (++) xs

dagToTree :: (Ord a) => a -> Graph a -> Tree a
dagToTree root g = go root
    where
        go r = Node r . map (fmap go) . neighbors r $ g

neighbors :: (Ord a) => a -> Graph a -> [(Integer,a)]
neighbors v (Graph es) = M.findWithDefault [] v $ es
