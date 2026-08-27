import Control.Applicative hiding(many)
import Parsing

{- 
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

-- Ejercicio 3 

trans :: Parser a -> Parser a
trans p = do     symbol "("
                 x <- p
                 symbol ")"
                 return x
                <|> p

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

-- Ejercicio 5

data Basetype = DInt | DChar | DFloat
  deriving(Show)
type Hasktype = [Basetype]

-- a->b->c => [Da,Db,Dc]

basetype :: Parser Basetype
basetype = do x <- symbol "Int"
              return DInt
            <|> do x <- symbol "Char"
                   return DChar
                <|> do x <- symbol "Float"
                       return DFloat
                    <|> failure

subtype :: Parser Hasktype
subtype = sepBy basetype (do x <- symbol "->"; return () )

-- Ejercicio 6 

{-
Escribir un parser para listas heterogeneas de enteros y caracteres por extension usando el formato de Haskell.
Defina un tipo de datos adecuado para representar estas listas parseadas. Por ejemplo, una cadena a parsear es la
siguiente: [1,'a','b',2,3,'c'].
-}

het' :: Parser Basetype
het' = do x <- digit
          return DInt
        <|> do symbol "'"
               x <- letter
               symbol "'"
               return DChar
               
            

het :: Parser Hasktype
het = do symbol "["
         x <- sepBy het' (do y <- symbol ","; return ())
         symbol "]"
         return x    
       <|> sepBy het' (do y <- symbol ","; return ())


 -}

-- Ejercicio 7



data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype
  deriving(Show)

basetype :: Parser Hasktype
basetype = do x <- symbol "Int"
              return DInt
            <|> do x <- symbol "Char"
                   return DChar
                <|> do x <- symbol "Float"
                       return DFloat
                    <|> failure

hasktype :: Parser Hasktype
hasktype = do x <- basetype
              do symbol "->"
                 y <- hasktype
                 return (Fun x y)
                <|> return x
            <|> do symbol "("
                   x <- hasktype
                   symbol ")"
                   do symbol "->"
                      y <- hasktype
                      return (Fun x y)
                    <|> return x
                      
-- Ejercicio 9

data TypeSpecifier = CInt | CChar | CFloat deriving (Show)
newtype Cst = C Int deriving(Show)
data DirectDeclarator = DD DirectDeclarator [Cst] | DT (DirectDeclarator) | Id String deriving(Show)
data Declarator = Ptr Declarator | DE DirectDeclarator deriving(Show)
data Declaration = D TypeSpecifier Declarator deriving(Show)


decl :: Parser Declaration
decl = do ts <- typeSpecifier
          d <- declarator
          symbol ";"
          return (D ts d)


typeSpecifier :: Parser TypeSpecifier
typeSpecifier = do symbol "int"
                   return CInt
                   <|> do symbol "char"
                          return CChar
                        <|> do symbol "float"
                               return CFloat
                              <|> failure

declarator :: Parser Declarator
declarator = do symbol "*"
                x <- declarator
                return (Ptr x)
              <|> do x <- dirDeclarator
                     return (DE x)

dirDeclarator :: Parser DirectDeclarator
dirDeclarator = do b <- dirDeclarator'
                   do symbol "["
                      c <- cst
                      symbol "]"
                      return (DD b [c])
                    <|> return b

dirDeclarator' :: Parser DirectDeclarator
dirDeclarator' = do symbol "("
                    d <- dirDeclarator
                    symbol ")"
                    return (DT (d))
                  <|> do x <- identifier
                         return (Id x)

cst :: Parser Cst
cst = do n <- natural
         return (C n)