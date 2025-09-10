data Expr = Num Int | BinOp Op Expr Expr

data Op = Add | Mul | Min | Div

-- Gramatica dada.
-- expr -> expr (’+’ term | ’-’ term) | term
-- term -> term (’*’ factor | ’/’ factor) | factor
-- factor -> ’(’ expr ’)’ | n

-- Gramatica corregida sin recursion izquierda.
-- expr -> term expr2
-- expr2 -> (’-’ term | ’+’ term) expr2 | e

-- t :: Expr
-- f :: Expr -> Expr
expr :: Parser Expr
expr = do
        t <- term
        f <- expr'
        return (f t)

expr' :: Parser (Expr -> Expr)
expr' = pMin <|> pSum <|> pEmpty
    
pMin :: Parser(Expr -> Expr)
pMin = do symbol "-"
          t <- term
          f <- expr'
          return (\e -> (f (BinOp Min e t)))
--------------------------
expr :: Parser Expr
expr = do t <- term
          e <- expr'
