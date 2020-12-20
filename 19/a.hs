import qualified Data.Array.ST as MA
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STR
import Control.Monad (forM_,when)
flap ff x = (\f -> f x) <$> ff
splitOn x xs = go xs []
    where
        go [] acc = [reverse acc]
        go (y:ys) acc =
            if x == y
            then reverse acc : go ys []
            else go ys (y:acc)

type Var = Int
type Terminal = Char

data Production
    = Inner Var Var Var
    | Unit Var Var
    | Leaf Var Terminal
    deriving Show

type Grammar = [Production]

unit :: Grammar -> Grammar
unit = go []
    where
        go g' [] = g'
        go g' (Unit a b : g) = go (blowup g') (blowup g)
            where
                blowup ps =
                    [ Inner a c d | Inner b' c d <- ps, b' == b ] ++
                    [ Unit a c    | Unit  b' c   <- ps, b' == b ] ++
                    [ Leaf a t    | Leaf  b' t   <- ps, b' == b ] ++
                    ps
        go g' (p:g) = go (p:g') g

readGrammar :: [String] -> Grammar
readGrammar = unit . concatMap readProduction

readProduction :: String -> [Production]
readProduction s =
    let (v,':':s') = break (== ':') s
        a = read v
        readRhs rhs = case words rhs of
            ['"':t:'"':""] -> Leaf  a t
            [b]            -> Unit  a (read b)
            [b,c]          -> Inner a (read b) (read c)
    in readRhs <$> splitOn '|' s'

input :: String -> (Int,Grammar,[String])
input s =
    let (g,"":ss) = break (== "") . lines $ s
    in (length g,readGrammar g,ss)

solve :: (Int,Grammar,[String]) -> Int
solve (r,g,ss) = length . filter cyk $ ss
    where
        cyk :: String -> Bool
        cyk i = let n = length i in ST.runST $ do
            arr <- MA.newArray ((1,1,0),(n,n,r-1)) False :: ST.ST s (MA.STUArray s (Int,Int,Var) Bool)

            forM_ [1..n] $ \s ->
                forM_ [ v | Leaf v t <- g, t == i !! pred s ] $ \v ->
                    MA.writeArray arr (1,s,v) True

            forM_ [2..n] $ \l ->
                forM_ [1..n-l+1] $ \s ->
                    forM_ [1..l-1] $ \p ->
                        forM_ [ (a,b,c) | Inner a b c <- g ] $ \(a,b,c) -> do
                            bMatches <- MA.readArray arr (p,s,b)
                            cMatches <- MA.readArray arr (l-p,s+p,c)
                            when (bMatches && cMatches) $
                                MA.writeArray arr (l,s,a) True
            MA.readArray arr (n,1,0)

main = interact $ show . solve . input
