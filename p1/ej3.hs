import Control.Applicative hiding(many)
import Parsing

-- Ejercicio 3 

trans :: Parser a -> Parser a
trans p = do     symbol "("
                 x <- p
                 symbol ")"
                 return x
                <|> p
