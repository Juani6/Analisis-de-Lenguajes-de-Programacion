import Control.Applicative hiding(many)
import Parsing

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
