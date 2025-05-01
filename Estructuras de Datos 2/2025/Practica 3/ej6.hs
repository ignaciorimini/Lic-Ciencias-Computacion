data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

-- Función principal: evaluador seguro.
seval :: Exp -> Maybe Int
seval (Lit n) = Just n
seval (Add e1 e2) = evalBinOp (+) e1 e2
seval (Sub e1 e2) = evalBinOp (-) e1 e2
seval (Prod e1 e2) = evalBinOp (*) e1 e2
seval (Div e1 e2) = evalDiv e1 e2

-- Función auxiliar para evitar repetir código. Crea un Maybe Int con la operación dada.
evalBinOp :: (Int -> Int -> Int) -> Exp -> Exp -> Maybe Int
evalBinOp op e1 e2 =
    case seval e1 of
        Nothing -> Nothing
        Just v1 -> case seval e2 of
                        Nothing -> Nothing
                        Just v2 -> Just (op v1 v2)

-- Función auxiliar para la división. Chequea si el segundo argumento es 0.
evalDiv :: Exp -> Exp -> Maybe Int
evalDiv e1 e2 =
    case seval e1 of
        Nothing -> Nothing
        Just v1 -> case seval e2 of
                        Nothing -> Nothing
                        Just 0 -> Nothing
                        Just v2 -> Just (div v1 v2)

-- Expresiones de ejemplo y prueba.
exp1 = Lit 5
exp2 = Add (Lit 4) (Lit 3)
exp3 = Sub (Lit 10) (Lit 7)
exp4 = Prod (Lit 5) (Lit 3)
exp5 = Div (Lit 30) (Lit 5)
exp6 = Add (Prod (Lit 3) (Lit 5)) (Div (Lit 6) (Lit 3))
exp7 = Div (Lit 5) (Lit 0)