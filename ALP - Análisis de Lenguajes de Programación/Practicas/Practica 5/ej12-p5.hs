-- 12) Escribir las leyes de las mónadas usando la notación do.

-- Recordatorio.
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- return (f x1 x2 ... xn)

-- do   x1 <- m1
--      x2 <- m2
--      return (f x1 x2 ... xn)

--------------------------------
-- Monad.1: return a >>= k = k a

do  x <- return a
    k x
-- Es equivalente a k a


-- Monad.2: m >>= return = m

do  x <- m
    return x
-- Es equivalente a m


-- Monad.3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h

do  y <- (do x <- m
             k x)
    h y

-- Es equivalente a:
do  x <- m
    y <- k x
    h y

