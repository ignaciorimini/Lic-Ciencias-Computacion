{-
Defina un evaluador eval :: Exp -> Int para el siguiente tipo algebraico:

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp
-}

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int
eval (Lit n) = n
eval (Add n1 n2) = (eval n1) + (eval n2)
eval (Sub n1 n2) = (eval n1) - (eval n2)
eval (Prod n1 n2) = (eval n1) * (eval n2)
eval (Div n1 n2) = div (eval n1) (eval n2)

-- exp1 = Add (Prod (Lit 5) (Lit 4)) (Sub (Lit 25) (Lit 5))
-- eval exp1 = 40