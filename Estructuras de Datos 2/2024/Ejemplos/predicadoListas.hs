module Test where

-- Tomar los números pares de una lista.
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

esLetraA :: Char -> Bool
esLetraA x = x == 'a'

tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras p [] = []
tomarMientras p (x:xs)
    | p x       = x : tomarMientras p xs
    | otherwise = []

eliminarMientras :: (a -> Bool) -> [a] -> [a]
eliminarMientras p [] = []
eliminarMientras p (x:xs)
    | p x       = eliminarMientras p xs
    | otherwise = x:xs

dividir :: (a -> Bool) -> [a] -> ([a], [a])
dividir p [] = ([], [])
dividir p (x:xs)
    | p x       = (x : xs1, xs2)
    | otherwise = (xs1, x : xs2)
    where
        (xs1, xs2) = dividir p xs

-- tomarMientras esPar [2, 4, 6, 8, 9] devuelve [2, 4, 6, 8]
-- tomarMientras esLetraA ['a', 'a', 'a', 'b', 'a', 'a'] devuelve ['a', 'a', 'a']
-- eliminarMientras esPar [2, 4, 6, 1, 3, 5] devuelve [1, 3, 5]
-- eliminarMientras esPar [1, 4, 6, 1, 3, 5] devuelve [4, 6, 1, 3, 5]
-- dividir esPar [2, 4, 6, 1, 3, 5] devuelve ([2,4,6],[1,3,5])

-- dividir esPar [2, 4, 6, 1, 3, 5]
-- p (2 : [4, 6, 1, 3, 5])
-- p 2 es verdadero -> (2 : dividir p [4, 6, 1, 3, 5], xs2)