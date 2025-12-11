-- 7) El tipo de datos Cont r a representa continuaciones en las que
-- dado el resultado de una función (de tipo a) y la continuación de 
-- la computación (a -> r), devuelve un valor en r.
-- Probar que Cont r es una mónada.

-- Cont :: ((a -> r) -> r) -> Cont r a
-- runCont :: Cont r a -> ((a -> r) -> r)
data Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
    -- return :: a -> Cont r a
    -- Recibe la continuación (k :: (a -> r)) y le aplica directamente
    -- el valor (x). La computación termina inmediatamente con k x.
    return x = Cont (\k -> k x)

    -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
    -- k' :: b -> r
    -- h :: (a -> r) -> r, no podemos aplicar directamente h k
    -- Por eso necesitamos h (k_mid), con k_mid :: a -> r.
    -- f x :: Cont r b, y luego runCont (f x) :: (b -> r) -> r.
    -- Aplicando el k' :: b -> r, entonces obtenemos el tipo r.
    -- Finalmente, obtenemos Cont ((b -> r) -> r)
    (Cont h) >>= f = Cont (\k' -> h (\x -> runCont (f x) k'))


-------------------------------------
-- Siempre que te enfrentes a una monada en un examen:
-- a) ¿Qué dato representa el estado o el contexto? (En State s a, es s. 
-- En Cont r a, es la función a -> r).

-- b) ¿Cómo se propaga ese dato en >>=? (En State, el s' se pasa a la 
-- siguiente acción. En Cont, el k' se pasa hacia atrás a la primera 
-- acción, envuelto en una función que espera el resultado $x$).