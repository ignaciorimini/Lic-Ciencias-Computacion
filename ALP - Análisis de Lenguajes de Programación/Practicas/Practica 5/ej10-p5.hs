-- 10) Escribir el siguiente fragmento de programa en 
-- términos de >>= y return.

-- Recordatorio.
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- return (f x1 x2 ... xn)

-- do   x1 <- m1
--      x2 <- m2
--      return (f x1 x2 ... xn)

--------------------------------------
do  x <- (do z <- y
            w <- f z
            return (g w z))
    y <- h x 3
    if y    
        then return 7
        else do z <- h x 2
                return (k z)

(y >>= \z ->
 f z >>= \w ->
 return (g w z)
) 
>>= \x ->
 h x 3 >>= \y' ->
 if y' 
    then return 7
    else 
        (h x 2 >>= \z' ->
        return (k z'))
            
