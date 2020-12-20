import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Data.List (foldl')

data Expr
    = Val Integer
    | Expr Expr Op Expr
    deriving (Show)

data Op = Add | Mult deriving (Show)

exprs = expr `P.endBy` P.char '\n'
expr = do
    h <- fac
    t <- P.many expr'
    return $ foldl' (\e1 e2 -> Expr e1 Mult e2) h t
expr' = P.try $ P.spaces >> P.char '*' >> P.spaces >> fac
fac = do
    h <- val
    t <- P.many fac'
    return $ foldl' (\e1 e2 -> Expr e1 Add e2) h t
fac' = P.try $ P.spaces >> P.char '+' >> P.spaces >> val
val = P.between (P.char '(') (P.char ')') expr
    <|> Val . read . (:[]) <$> P.digit

main = interact $ show . sum . map eval . either (error . show) id . P.parse exprs "stdin"

eval :: Expr -> Integer
eval (Val n) = n
eval (Expr e1 Add  e2) = eval e1 + eval e2
eval (Expr e1 Mult e2) = eval e1 * eval e2
