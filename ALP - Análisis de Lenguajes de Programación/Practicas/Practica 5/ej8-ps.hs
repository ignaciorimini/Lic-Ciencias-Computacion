-- 8) Demostrar que el constructor de tipo [] (lista) es una mónada.

-- Para demostrar que es una mónada, debemos dar una instancia Monad
-- Y verificar que se cumplen las leyes monádicas.

instance Monad [] where
    -- return :: a -> m a
    return x = [x]

    -- (>>=) :: m a -> (a -> m b) -> m b
    xs >>= f = concat (map f xs)

-----------
-- Monad.1: return a >>= k = k a
-- = return [a] >>= k
-- = [a] >>= k                      <-- def return
-- = concat (map k [a])             <-- def >>=
-- = concat [k a]                   <-- def map
-- = k a                            <-- def concat

-----------
-- Monad.2: m >>= return = m
-- Sea m = [x1, x2, ... xm]
-- = m >>= return
-- = concat (map return m)                          <-- def >>=
-- = concat (map return [x1, x2, ... xm])           <-- def m
-- = concat [return x1, return x2, ... return xm]   <-- def map
-- = concat [[x1], [x2], ... [xm]]                  <-- def return
-- = [x1, x2, ... xm]

-----------
-- Monad.3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- = (xs >>= k) >>= h
-- = (concat (map k xs)) >>= h
-- = concat (map h (concat (map k xs)))

-- = xs >>= (\x -> k x >>= h)
-- = concat (map (\x -> k x >>= h) xs)
-- = concat (map h (concat map k xs))