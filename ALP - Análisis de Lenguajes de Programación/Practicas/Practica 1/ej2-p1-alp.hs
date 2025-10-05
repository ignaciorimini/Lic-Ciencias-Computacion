import Parsing
import Control.Applicative

-- Extender el parser de expresiones visto en clase para permitir el uso de la resta y la división, basándose en la siguiente extensión a la gramática:
-- expr -> term ('+' expr | '-' expr | e)
-- term -> factor ('*' term | '/' term | e)
-- factor -> '(' exp ')' | n
-- n -> d | dn
-- d -> '0' | '1' | ... | '9'

expr :: Parser Int
expr = 
    do t <- term
       (do  symbol "+"
            e <- expr
            return (t+e)
        <|> do  symbol "-"
                e <- expr
                return (t+e))
        <|> return t

term :: Parser Int
term = 
    do f <- factor
       (do  symbol "*"
            t <- term
            return (f*t)
        <|> do  symbol "/"
                t <- term
                return (div f t))
        <|> return f

factor :: Parser Int
factor = 
    do  symbol "("
        e <- expr
        symbol ")"
        return e
    <|> natural

eval :: String -> Int
eval xs = fst (head (parse expr xs))

-- Ejemplos.
-- eval "2*3+4" -> 10
-- eval "(2*3)+(12/4)*(8/2)" -> 18