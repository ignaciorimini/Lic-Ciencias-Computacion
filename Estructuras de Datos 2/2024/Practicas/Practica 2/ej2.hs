-- Dar el tipo de las siguientes operaciones y explicar su propósito.
-- a) (+5)
-- Tipo: :: Num a => a -> a
-- Propósito: es una función parcial que suma 5 a cualquier valor que se le pase como argumento. Por ejemplo, (+5) 5 = 10.


-- b) (0<)
-- Tipo: :: Ord a => a -> Bool
-- Propósito: es una función que recibe un argumento de tipo Ord y devuelve True si el argumento es mayor a 0 y False en caso contrario.


-- c) ('a':)
-- Tipo: [Char] -> [Char]
-- Propósito: toma un String y devuelve un String con la primer letra 'a' ++ el argumento pasado.


-- d) (++ "\n")
-- Tipo: :: [Char] -> [Char]
-- Propósito: recibe un String como argumento y devuelve el mismo String concantenándole "\n" al final.

-- (++ "\n") "hola"
-- "hola\n"


-- e) filter (== 7)
-- Tipo: :: Eq a => [a] -> [a]
-- Propósito: recibe una lista de elementos comparables y evalúa el predicado lista[i] == 7 para cada elemento. Finalmente, devuelve una lista con todos aquellos elementos que dieron True en el predicado, es decir, una lista con todos los 7 que haya en la lista pasada como argumento.

-- filter (== 7) [1, 2, 7, 7, 7, 8]
-- [7,7,7]


-- f) map (++[1])
-- Tipo: :: Num a => [[a]] -> [[a]]
-- Propósito: recibe una lista de listas de números y devuelve una nueva lista de listas donde a cada lista interna le agrega el elemento 1 al final.

-- map (++ [1]) [[1], [2]]
-- [[1,1],[2,1]]