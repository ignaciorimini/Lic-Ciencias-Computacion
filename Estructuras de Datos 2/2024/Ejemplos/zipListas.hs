module Test where

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- zip' [1,2,3] ['a', 'b', 'c'] -> [(1,'a'),(2,'b'),(3,'c')]


-- Lista de pares de elementos adyacentes
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- pairs [1,2,3,4] -> [(1,2), (2,3), (3,4)]


-- Recibe una lista de elementos pertenecientes a la clase Ord (ordenables) y devuelve True si la lista está ordenada y False en caso contrario.
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- pairs xs: Esta parte de la expresión genera una lista de pares de elementos adyacentes de la lista xs. Por ejemplo, si xs = [1, 2, 3, 4], pairs xs producirá [(1, 2), (2, 3), (3, 4)].
-- [x <= y | (x, y) <- pairs xs]: Esta es una comprensión de lista que genera una lista de valores booleanos. Para cada par (x, y) en la lista de pares generada por pairs xs, verifica si x es menor o igual que y. Esto produce una lista de valores booleanos que indican si cada par de elementos adyacentes está ordenado correctamente.
-- and [...]: La función and toma una lista de valores booleanos y devuelve True si todos los valores en la lista son True, y False en caso contrario. Por lo tanto, and se utiliza para verificar si todos los pares de elementos adyacentes están ordenados correctamente.


-- La función rangeof toma tres argumentos: low (el límite inferior del rango), hi (el límite superior del rango) y xs (una lista). Devuelve una nueva lista que contiene los elementos de xs que están en el rango desde low hasta hi, inclusive.
rangeof :: Int -> Int -> [a] -> [a]
rangeof low hi xs = [x | (x, i) <- zip xs [0..], i >= low, i <= hi]

-- zip xs [0..]: Esta parte de la expresión combina cada elemento de xs con su índice correspondiente, creando una lista de pares (x, i), donde x es un elemento de xs y i es su índice.
-- i >= low, i <= hi: Estas son las condiciones que limitan el rango. i es el índice del elemento actual en xs. Solo se seleccionan los elementos cuyos índices i están dentro del rango especificado por low y hi.