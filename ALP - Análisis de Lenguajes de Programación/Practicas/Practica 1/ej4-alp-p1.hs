data Expr = Num Int | BinOp Op Expr Expr
data Op = Add | Mul | Min | Div

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
expr :: Parser Expr
expr = do
        t <- term
        f <- expr'
        return (f t)

expr' :: Parser (Expr -> Expr)
expr' = pMin <|> pSum <|> pEmpty

pMin :: Parser (Expr -> Expr)
pMin = do 
        symbol "-"
        t <- term
        f <- expr'
        return (\e -> (f (BinOp Min e t)))

pSum :: Parser (Expr -> Expr)
pSum = do
        symbol "+"
        t <- term
        f <- expr'
        return (\e -> (f (BinOp Add e t)))
