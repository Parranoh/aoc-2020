import qualified Data.IntMap.Strict as M
import Data.Bifunctor (bimap,first)

class Monoid m => Group m where
    invert :: m -> m
    pow :: (Integral x) => m -> x -> m

    pow x0 n0 = case compare n0 0 of
        LT -> invert . f x0 $ negate n0
        EQ -> mempty
        GT -> f x0 n0
        where
            f x n
                | even n = f (x `mappend` x) (n `quot` 2)
                | n == 1 = x
                | otherwise = g (x `mappend` x) (n `quot` 2) x
            g x n c
                | even n = g (x `mappend` x) (n `quot` 2) c
                | n == 1 = x `mappend` c
                | otherwise = g (x `mappend` x) (n `quot` 2) (x `mappend` c)

newtype G = G { unG :: Int }
n :: Int
n = 20201227

instance Semigroup G where
    G a <> G b = G $ a * b `mod` n

instance Monoid G where
    mempty = G 1

instance Group G where
    invert (G a) = G $ euklid a n

euklid :: Int -> Int -> Int
euklid a n = go 0 1 n a
    where
        go t _ r 0
            | r > 1     = error "a is not invertible"
            | t < 0     = t + n
            | otherwise = t
        go t newt r newr =
            let quotient = r `div` newr
            in go newt (t - quotient * newt) newr (r - quotient * newr)

babyStepGiantStep :: G -> G -> Int
babyStepGiantStep a b =
    let m = ceiling . sqrt . fromIntegral $ n
        table =
            M.fromList .
            map (first unG) .
            take m .
            iterate (bimap (<> a) succ) $
            (mempty,0)
        a_m = pow a (-m)
        go i c = if i == m then error "loop ended" else
            case M.lookup (unG c) table of
                Nothing -> i `seq` go (succ i) (c <> a_m)
                Just j  -> i * m + j
    in go 0 b

main = interact $ output . solve . input

input :: String -> (G,G)
input s = let [a,b] = lines s in (G $ read a,G $ read b)

solve :: (G,G) -> G
solve (a,b) =
    let loopSizeA = babyStepGiantStep (G 7) a
    in pow b loopSizeA

output :: G -> String
output = show . unG
