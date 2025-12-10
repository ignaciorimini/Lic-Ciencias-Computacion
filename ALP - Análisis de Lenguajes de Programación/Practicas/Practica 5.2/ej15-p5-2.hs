-- 15) Definir las siguientes funciones:

----------------------------------------------------------------
-- mapM f xs aplica la función monádica f a cada elemento de lista xs,
-- retornando la lista de resultados encapsulada en la mónada.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) =
    do  b <- f x
        bs <- mapM' f xs
        return (b:bs)

-- f x >>= (\b -> mapM f xs >>= (\bs -> return (b:bs)))

----------------------------------------------------------------
-- Análoga a fold para listas, pero con su resultado encapsulado
-- en la mónada. Ejemplo: 
-- foldM f e1 [x1, x2, x3] =
--  do  e2 <- f e1 x1
--      e3 <- f e2 x2
--      f e3 x3
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x:xs) =
    do  new_acc <- f acc x
        foldM f new_acc xs


----------------------------------------------------------------
-- Ejemplos.
-- Una función monádica que suma 10 y falla si el número es 5.
safeIncrement :: Int -> Maybe Int
safeIncrement 5 = Nothing
safeIncrement n = Just (n + 10)

-- Función de plegado que suma y falla si el acumulador excede 10.
safeSumFold :: Int -> Int -> Maybe Int
safeSumFold acc x
    | acc + x > 10 = Nothing
    | otherwise    = Just (acc + x)

-- mapM' safeIncrement [1,2,3] -> Just [11,12,13]
-- mapM' safeIncrement [1,5,3] -> Nothing

-- foldM safeSumFold 0 [1,2,3,4] -> Just 10
-- foldM safeSumFold 0 [1,2,3,4,5] -> Nothing