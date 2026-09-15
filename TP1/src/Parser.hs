module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!" -- Not 
                        , "=" -- Let
                        , "=="
                        , "!="
                        , ";" -- Seq
                        , "," 
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

intterm :: Parser (Exp Int)
intterm = chainl1 atom termop

termop :: Parser (Exp Int -> Exp Int -> Exp Int)
termop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)

atom :: Parser (Exp Int)
atom = minusPar <|> varOpPar <|> natPar <|> varParen

minusPar :: Parser (Exp Int)
minusPar = do 
  reservedOp lis "-"
  x <- intexp
  return (UMinus x)

varOpPar :: Parser (Exp Int)
varOpPar = do 
  s <- identifier lis
  do t <- varOpParse 
     return (t s) 
   <|> return (Var s) 

varOpParse :: Parser (Variable -> Exp Int)
varOpParse = (reservedOp lis "++" >> return VarInc)
  <|> (reservedOp lis "--" >> return VarDec)

natPar :: Parser (Exp Int)
natPar = do 
  n <- natural lis
  return (Const (fromInteger n))

varParen :: Parser (Exp Int)
varParen = do 
  x <- (parens lis intexp)
  return x


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm boolop 

boolop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolop = do (reservedOp lis "&&" >> return And)
  <|> (reservedOp lis "||" >> return Or)

boolterm :: Parser (Exp Bool)
boolterm = notParse <|> boolOpParse <|> true <|> false 
  where true  = (reservedOp lis "true" >> return BTrue)
        false = (reservedOp lis "false" >> return BFalse) 

notParse :: Parser (Exp Bool)
notParse = do
  reservedOp lis "!"
  b <- boolterm
  return (Not b)

boolOpParse :: Parser (Exp Bool)
boolOpParse = do
  e1 <- intexp
  op <- boolterm'
  e2 <- intexp
  return (op e1 e2)  

boolterm' :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolterm' = (reservedOp lis "<" >> return Lt)
  <|> (reservedOp lis ">" >> return Gt)
  <|> (reservedOp lis "==" >> return Eq)
  <|> (reservedOp lis "!=" >> return NEq)
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm' seqPar
  where comm' = skipPar <|> letPar <|> ifThenPar <|> repeatPar 

seqPar :: Parser (Comm -> Comm -> Comm)
seqPar = reservedOp lis ";" >> return SeqPattern

skipPar :: Parser Comm
skipPar = reservedOp lis "skip" >> return Skip

letPar :: Parser Comm
letPar = do 
  name <- identifier lis
  reservedOp lis "="
  exp <- intexp 
  return (Let name exp)

ifThenPar :: Parser Comm
ifThenPar = do
  reservedOp lis "if"
  b <- boolexp
  reservedOp lis "then"
  c1 <- braces lis comm
  do reservedOp lis "else"
     c2 <- braces lis comm
     return (IfThenElse b c1 c2)
   <|> return (IfThen b c1)

repeatPar :: Parser Comm
repeatPar = do
  reservedOp lis "repeat"
  c1 <- braces lis comm
  reservedOp lis "until"
  b <- boolexp
  return (RepeatUntil c1 b)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
