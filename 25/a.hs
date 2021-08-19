{-# LANGUAGE ScopedTypeVariables #-}

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

data G a = G { unG :: Int }

data Zero
data B0 a
data B1 a
numPred0 :: B0 a -> a
numPred0 = undefined
numPred1 :: B1 a -> a
numPred1 = undefined
class Number a where
    numValue :: a -> Int
instance Number Zero where
    numValue = const 0
instance (Number a) => Number (B0 a) where
    numValue x = 2 * numValue (numPred0 x)
instance (Number a) => Number (B1 a) where
    numValue x = 2 * numValue (numPred1 x) + 1
type N = B1 (B1 (B0 (B1 (B0 (B0 (B0 (B0 (B1 (B1 (B1 (B1 (B1 (B1 (B0 (B0 (B0 (B0 (B1 (B0 (B1 (B1 (B0 (B0 (B1 Zero)))))))))))))))))))))))) -- 20201227

instance (Number a) => Semigroup (G a) where
    G x <> G y = G $ x * y `mod` numValue (undefined :: a)

instance (Number a) => Monoid (G a) where
    mempty = G 1

instance (Number a) => Group (G a) where
    invert (G x) = G $ euklid x (numValue (undefined :: a))

euklid :: Int -> Int -> Int
euklid x n = go 0 1 n x
    where
        go t _ r 0
            | r > 1     = error "not invertible"
            | t < 0     = t + n
            | otherwise = t
        go t newt r newr =
            let quotient = r `div` newr
            in go newt (t - quotient * newt) newr (r - quotient * newr)

babyStepGiantStep :: forall a. (Number a) => G a -> G a -> Int
babyStepGiantStep b x =
    let m = ceiling . sqrt . fromIntegral $ numValue (undefined :: a)
        table =
            M.fromList .
            map (first unG) .
            take m .
            iterate (bimap (<> b) succ) $
            (mempty,0)
        b_m = pow b (-m)
        go i c = if i == m then error "discrete log does not exist" else
            case M.lookup (unG c) table of
                Nothing -> i `seq` go (succ i) (c <> b_m)
                Just j  -> i * m + j
    in go 0 x

main = interact $ output . solve . input

input :: String -> (G N,G N)
input s = let [a,b] = lines s in (G $ read a,G $ read b)

solve :: (Number a) => (G a,G a) -> G a
solve (x,y) =
    let loopSizeX = babyStepGiantStep (G 7) x
    in pow y loopSizeX

output :: G a -> String
output = show . unG
