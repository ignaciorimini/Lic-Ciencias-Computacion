import Parsing
import Control.Applicative hiding (many)

-- Modificar el parser del ejercicio 2 para que en lugar de evaluar una expresión genere un árbol de sintaxis abstracta dado por el tipo:
data Expr = Num Int | BinOp Op Expr Expr
data Op = Add | Mul | Min | Div

-- Gramática dada:
-- expr -> term ('+' expr | '-' expr | e)
-- term -> factor ('*' term | '/' term | e)
-- factor -> '(' exp ')' | n
-- n -> d | dn
-- d -> '0' | '1' | ... | '9'

expr :: Parser Expr
expr = do t <- term
          f <- expr'
          return ...

expr' :: Parser ...
expr' = pPlus <|> pMin <|> pEmpty

pPlus
