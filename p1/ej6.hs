import Control.Applicative hiding(many)
import Parsing

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
