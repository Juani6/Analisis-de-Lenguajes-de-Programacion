import Control.Applicative hiding(many)
import Parsing
                      
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