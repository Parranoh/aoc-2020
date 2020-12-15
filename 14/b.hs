import Numeric (readInt)
import Data.Bits ((.&.),(.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Bifunctor (bimap)
import Control.Monad (join)
flap ff x = (\f -> f x) <$> ff
both = join bimap

type Mask = Integer -> [Integer]
type Memory = M.Map Integer Integer

readMask :: String -> Mask
readMask = flap . map (\(l,h) -> (.&.) l . (.|.) h) . map (both readBin) . choices

readBin :: String -> Integer
readBin = fst . head . readInt 2 isDigit (read . (:[]))

choices :: String -> [(String,String)]
choices "" = [("","")]
choices ('X':ds) =
    let rec = choices ds
    in map (both $ (:) '0') rec ++ map (both $ (:) '1') rec
choices (d:ds) = map (bimap ('1':) (d:)) . choices $ ds

isDigit :: Char -> Bool
isDigit = flip elem $ 'X':['0'..'9']

main = interact $ show . sum . snd . foldl' step ((:[]),M.empty) . lines

step :: (Mask,Memory) -> String -> (Mask,Memory)
step (mask,mem) line = case line !! 1 of
    'a' -> (readMask . drop 7 $ line,mem)
    'e' ->
        let
            addrs = mask . read . takeWhile isDigit . drop 4 $ line
            value = read . drop 2 . dropWhile (/= '=') $ line
        in (mask,foldl' (\m a -> M.insert a value m) mem addrs)
