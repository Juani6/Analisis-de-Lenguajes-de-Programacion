import Control.Applicative hiding(many)
import Parsing

data GenTree a = N a [GenTree a] deriving (Show)

tree :: Parser (GenTree Int)
tree = do symbol "("
          symbol "N"
          n <- natural
          ts <- many tree
          symbol ")"
          return (N n ts)

--"(N 2 (N 1 (N 5) (N 3)) (N 9) (N 4 (N 7)))"