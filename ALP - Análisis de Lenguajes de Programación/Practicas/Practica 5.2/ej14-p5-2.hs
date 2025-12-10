-- 14) Sea M una mónada. Dados los operadores:
-- (>>) :: M a -> M b -> M b
-- (>>=) :: M a -> (a -> M b) -> M b

---------------------
-- a) De ser posible, escribir (>>) en función de (>>=).

(>>) :: M a -> M b -> M b
ma (>>) mb = ma >>= (\x -> mb)

-- El resultado es la ejecución secuencial de ma seguida de mb, 
-- con el resultado de ma descartado.

---------------------
-- b) De ser posible, escribir (>>=) en función de (>>).

(>>=) :: M a -> (a -> M b) -> M b
-- NO SE PUEDE.
-- (>>=) debe ser la primitiva porque es el único operador que
-- captura el valor del resultado de la primera acción para determinar 
-- la segunda acción.