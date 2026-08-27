import Control.Applicative hiding(many)
import Parsing

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
