import qualified Data.Array.ST as MA
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STR
import Control.Monad (forM_)

main = interact $ show . solve . map read . words . map commaToSpace

commaToSpace :: Char -> Char
commaToSpace ',' = ' '
commaToSpace c   = c

solve :: [Int] -> Int
solve start = ST.runST $ do
    time <- STR.newSTRef 0 :: ST.ST s (STR.STRef s Int)
    lastSaid <- STR.newSTRef 0 :: ST.ST s (STR.STRef s Int)
    lastSaidTimes <- MA.newArray (0,2020) 0 :: ST.ST s (MA.STUArray s Int Int)
    forM_ start $ \n -> do
        t <- STR.readSTRef time
        STR.writeSTRef time $ succ t
        ln <- STR.readSTRef lastSaid
        STR.writeSTRef lastSaid n
        MA.writeArray lastSaidTimes ln t
    let go = do
        t <- STR.readSTRef time
        ln <- STR.readSTRef lastSaid
        lt <- MA.readArray lastSaidTimes ln
        let nt = if lt == 0 then 0 else t - lt
        STR.writeSTRef time $ succ t
        STR.writeSTRef lastSaid nt
        MA.writeArray lastSaidTimes ln t
        if succ t == 2020 then return nt else go
    go
