-- 9) Dado el siguiente tipo de datos para representar 
-- expresiones matemáticas.

data Expr a = Var a | Num Int | Add (Expr a) (Expr a)

-- a) Dar la instancia de Monad para Expr y probar que es una mónada.
instance Monad Expr where
    -- return :: a -> m a
    return x = Var x

    -- (>>=) :: m a -> (a -> m b) -> m b
    ... >>= ... = 