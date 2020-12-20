import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Data.List (foldl')

data Expr
    = Val Integer
    | Expr Expr Op Expr

data Op = Add | Mult

expr = do
    h <- val
    t <- P.many expr'
    return $ foldl' (\e1 (o,e2) -> Expr e1 o e2) h t
expr' = do
    P.spaces
    o <- op
    P.spaces
    e2 <- val
    return (o,e2)
val = P.between (P.char '(') (P.char ')') expr
    <|> Val . read . (:[]) <$> P.digit
op = const Add <$> P.char '+'
    <|> const Mult <$> P.char '*'

main = interact $ show . sum . map (eval . either (error . show) id . P.parse expr "stdin") . lines

eval :: Expr -> Integer
eval (Val n) = n
eval (Expr e1 Add  e2) = eval e1 + eval e2
eval (Expr e1 Mult e2) = eval e1 * eval e2
