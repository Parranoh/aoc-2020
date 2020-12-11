import qualified Data.Array.ST as MA
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IA
import qualified Control.Monad.ST as ST

data Instruction
    = Jmp Int
    | Acc Integer
    | Nop
    deriving (Show)

type Program = IA.Array Int Instruction

main = interact $ show . findLoop . readProgram

readProgram :: String -> Program
readProgram = (\is -> IA.listArray (1,length is) is) . map readInstruction . lines

readInstruction :: String -> Instruction
readInstruction (a:b:c:_:s:num) = case [a,b,c] of
    "jmp" -> Jmp num'
    "acc" -> Acc num'
    "nop" -> Nop
    where
        num' :: (Num a,Read a) => a
        num' = (if s == '-' then negate else id) $ read num

findLoop :: Program -> Integer
findLoop prog = ST.runST $ do
    visited <- MA.newArray (IA.bounds prog) False :: ST.ST s (MA.STUArray s Int Bool)
    go visited 1 0
    where
        go :: MA.STUArray s Int Bool -> Int -> Integer -> ST.ST s Integer
        go visited pc acc = do
            v <- MA.readArray visited pc
            if v then return acc else do
                MA.writeArray visited pc True
                case prog ! pc of
                    Jmp n -> go visited (pc + n) acc
                    Acc n -> go visited (succ pc) (acc + n)
                    Nop   -> go visited (succ pc) acc
