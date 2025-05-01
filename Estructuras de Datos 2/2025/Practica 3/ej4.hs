data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = div (eval e1) (eval e2)

-- Expresiones de ejemplo y prueba.
exp1 = Lit 5
exp2 = Add (Lit 4) (Lit 3)
exp3 = Sub (Lit 10) (Lit 7)
exp4 = Prod (Lit 5) (Lit 3)
exp5 = Div (Lit 30) (Lit 5)
exp6 = Add (Prod (Lit 3) (Lit 5)) (Div (Lit 6) (Lit 3))
exp7 = Div (Lit 5) (Lit 0)