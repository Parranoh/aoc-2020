import Text.Read (readMaybe)
import Data.List (sort,foldl1')
import Data.Maybe (catMaybes)

main = interact $ show . solve . nas . words . map commaToSpace . last . lines

commaToSpace :: Char -> Char
commaToSpace ',' = ' '
commaToSpace c   = c

solve :: [(Integer,Integer)] -> Integer
solve = snd . foldl1' g
    where
        g (ns,x) (n,a) = (ns * n,x')
            where
                x' = head . filter ((== a) . (`mod` n)) . iterate (+ ns) $ x

nas :: [String] -> [(Integer,Integer)]
nas = reverse . sort . catMaybes . zipWith f [0..] . map readMaybe
    where
        f a = fmap (\id -> (id,negate a `mod` id))
