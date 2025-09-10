import Parsing
import Control.Applicative

-- ___________________________________________________________________________________
-- Consideremos la siguiente gramática libre de contexto.
-- exp -> term '+' exp | term
-- term -> atom '*' term | atom
-- atom -> '(' exp ')' | n
-- n -> d | dn
-- d -> '0' | '1' | ... | '9'

-- Simplificamos la gramática factorizando las reglas para expr y term.
-- expr -> term ('+' expr | '-' expr | e)
-- term -> atom ('*' term | '/' term | e)

-- Definimos el parser para la gramática.
expr :: Parser Int
expr = 
  do t <- term
     (do  symbol "+"
          e <- expr
          return (t + e) 
       <|> do symbol "-"
              e <- expr
              return (t - e))
       <|> return t

term :: Parser Int
term =
  do f <- factor
     (do symbol "*"
         t <- term
         return (f * t) 
      <|> do  symbol "/"
              t <- term
              return (f `div` t)) 
      <|> return f

factor :: Parser Int
factor =
  do symbol "("
     e <- expr
     symbol ")"
     return e
  <|> natural

eval :: String -> Int
eval xs = fst (head (parse expr xs))