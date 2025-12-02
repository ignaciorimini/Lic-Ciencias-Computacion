-- 11) Escribir el siguiente fragmento de programa monádico
-- usando notación do.

(m >>= \x -> h x) >>= \y -> f y >>= \z -> return (g z)

do  y <- (do x <- m
             h x)
    z <- f y
    return (g z)

-- Recordatorio.
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- return (f x1 x2 ... xn)

-- do   x1 <- m1
--      x2 <- m2
--      return (f x1 x2 ... xn)