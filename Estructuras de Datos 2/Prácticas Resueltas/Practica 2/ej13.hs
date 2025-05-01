-- Definir las siguientes funciones usando listas por comprensión.

-- a) Dado un entero positivo x devuelve la lista de los divisores de x (y la lista vacía si el entero no es positivo).
divisors :: Int -> [Int]
divisors n  | n >= 0 = [d | d <- [1..n], mod n d == 0 ]
            | otherwise = []

-- Devuelve True si un numero es primo, False en caso contrario; haciendo uso de la función divisors.
primo :: Int -> Bool
primo n | n >= 0 = length (divisors n) <= 2
        | otherwise = length (divisors (-n)) <= 2

-- b) Dados un entero x y una lista de enteros, descarta la lista de los elementos distintos a x.
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- c) Dado un natural n, devuelve las cuadruplas (a,b,c,d) con 0 < a,b,c,d <= n que cumplen a^2 + b^2 = c^2 + d^2
cuadruplas :: Int -> [(Int, Int, Int, Int)]
cuadruplas n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n] , a^2 + b^2 == c^2 + d^2]

-- d) Dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], x `notElem` take i xs]
-- take i xs toma los elementos de antes del índice actual para preguntar si x ya ha aparecido antes.