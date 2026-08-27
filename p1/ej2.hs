import Control.Applicative hiding(many)
import Parsing
 
expr :: Parser Int
expr = do t <- term
          do e <- expr' t
             return e

expr' :: Int -> Parser Int
expr' t = do symbol "+"
             t' <- term
             e <- expr' t'
             return (t+e)
            <|> do symbol "-"
                   t' <- term
                   e <- expr' t'
                   return (e-t)
                  <|> return t
            
term :: Parser Int
term = do f <- factor
          t <- term' f
          return t

term' :: Int -> Parser Int
term' f = do symbol "*"
             f' <- factor
             t <- term' f'
             return (f*t)
            <|> do symbol "/"
                   f' <- factor
                   t <- expr' f'
                   return (div t f)
                  <|> return f
   
factor :: Parser Int
factor = do symbol "(" 
            e <- expr
            symbol ")"
            return e
           <|> integer


eval :: String -> Int
eval xs = fst (head (parse expr xs))
