-- Dar el tipo de las siguientes operaciones y explicar su propósito.

-- Suma 5 a un número dado.
-- (+5) :: Num a => a -> a
-- (+5)

-- Devuelve True si un número dado es mayor a 0, y False en caso contrario.
-- (0<) :: (Ord a, Num a) => a -> Bool
-- (0<)

-- Agrega el caracter 'a' al principio de un string.
-- ('a':) :: [Char] -> [Char]
-- ('a':)

-- Agrega el caracter "\n" al final de un string.
-- (++ "\n") :: [Char] -> [Char]
-- (++ "\n")

-- Dada una lista de valores numéricos, devuelve todas las ocurrencias que sean iguales a 7.
-- filter (== 7) :: (Eq a, Num a) => [a] -> [a]
-- filter (== 7)

-- Recibe una lista de listas de números y le agrega el número 1 al final de cada una de ellas.
-- map (++ [1]) :: Num a => [[a]] -> [[a]]
-- map (++ [1])