import Control.Applicative hiding(many)
import Parsing

-- Ejercicio 4 
data Expr = Num Int | BinOp Op Expr Expr
  deriving(Show)
data Op = Add | Mul | Min | Div
  deriving(Show)

expr :: Parser Expr
expr = do t <- term
          do e <- expr' t
             return e

expr' :: Expr -> Parser Expr
expr' t = do symbol "+"
             t' <- term
             e <- expr' t'
             return (BinOp Add t e)
            <|> do symbol "-"
                   t' <- term
                   e <- expr' t'
                   return (BinOp Min t e)
                  <|> return t
            
term :: Parser Expr
term = do f <- factor
          t <- term' f
          return t

term' :: Expr -> Parser Expr
term' f = do symbol "*"
             f' <- factor
             t <- term' f'
             return (BinOp Mul t f)
            <|> do symbol "/"
                   f' <- factor
                   t <- expr' f'
                   return (BinOp Div t f)
                  <|> return f
   
factor :: Parser Expr
factor = do symbol "(" 
            e <- expr
            symbol ")"
            return e
           <|> ntoast natural

ntoast :: Parser Int -> Parser Expr
ntoast p = do x <- p
              return (Num x)
            
eval2 :: String -> Expr
eval2 xs = fst (head (parse expr xs))
