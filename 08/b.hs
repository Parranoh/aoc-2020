import qualified Data.Array.ST as MA
import Data.Array.IArray ((!),(//))
import qualified Data.Array.IArray as IA
import qualified Control.Monad.ST as ST
import Data.Maybe (catMaybes)

data Instruction
    = Jmp Int
    | Acc Integer
    | Nop Int
    deriving (Show)

type Program = IA.Array Int Instruction

main = interact $ show . catMaybes . map run . fixes . readProgram

readProgram :: String -> Program
readProgram = (\is -> IA.listArray (1,length is) is) . map readInstruction . lines

readInstruction :: String -> Instruction
readInstruction (a:b:c:_:s:num) = case [a,b,c] of
    "jmp" -> Jmp num'
    "acc" -> Acc num'
    "nop" -> Nop num'
    where
        num' :: (Num a,Read a) => a
        num' = (if s == '-' then negate else id) $ read num

fixes :: Program -> [Program]
fixes prog = go 1
    where
        go i
            | i > len   = []
            | otherwise = case prog ! i of
                Jmp n -> prog // [(i,Nop n)] : go (succ i)
                Nop n -> prog // [(i,Jmp n)] : go (succ i)
                Acc _ ->                       go (succ i)
        len = snd . IA.bounds $ prog

run :: Program -> Maybe Integer
run prog = ST.runST $ do
    visited <- MA.newArray (IA.bounds prog) False :: ST.ST s (MA.STUArray s Int Bool)
    go visited 1 0
    where
        go :: MA.STUArray s Int Bool -> Int -> Integer -> ST.ST s (Maybe Integer)
        go visited pc acc = if pc == len + 1 then return $ Just acc else do
            v <- MA.readArray visited pc
            if v then return Nothing else do
                MA.writeArray visited pc True
                case prog ! pc of
                    Jmp n -> go visited (pc + n) acc
                    Acc n -> go visited (succ pc) (acc + n)
                    Nop _ -> go visited (succ pc) acc
        len = snd . IA.bounds $ prog
