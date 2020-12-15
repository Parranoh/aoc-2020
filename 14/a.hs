import Numeric (readInt)
import Data.Bits ((.&.),(.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M

type Mask = Integer -> Integer
type Memory = M.Map Integer Integer

readMask :: String -> Mask
readMask s = (.|.) ones . (.&.) zeros
    where
        ones,zeros :: Integer
        [(ones,_)]  = readInt 2 isDigit (\d -> case d of { 'X' -> 0 ; _ -> read [d] }) s
        [(zeros,_)] = readInt 2 isDigit (\d -> case d of { 'X' -> 1 ; _ -> read [d] }) s

isDigit :: Char -> Bool
isDigit = flip elem $ 'X':['0'..'9']

main = interact $ show . sum . snd . foldl' step (id,M.empty) . lines

step :: (Mask,Memory) -> String -> (Mask,Memory)
step (mask,mem) line = case line !! 1 of
    'a' -> (readMask . drop 7 $ line,mem)
    'e' ->
        let
            addr = read . takeWhile isDigit . drop 4 $ line
            value = mask . read . drop 2 . dropWhile (/= '=') $ line
        in (mask,M.insert addr (mask value) mem)
