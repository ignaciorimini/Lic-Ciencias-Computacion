-- Funciones para practicar listas por comprensión.

--------------------------------------------------
-- Nivel 1 – Básicos de filtrado y transformación.
-- Dada una lista, devuelve los elementos positivos de una lista.
positivos :: [Int] -> [Int]
positivos xs = [x | x <- xs, x > 0]

-- Dada una lista de enteros, devuelve el doble de cada número par de la lista.
doblesPares :: [Int] -> [Int]
doblesPares xs = [2*x | x <- xs, even x]

-- Dado un entero, devuelve la lista de cuadrados menores que un número dado.
cuadradosMenores :: Int -> [Int]
cuadradosMenores n = [x^2 | x <- [0..n], x^2 <= n]


--------------------------------------------------
-- Nivel 2 – Generación combinatoria.
-- Dado un entero, devuelve la lista de pares (x,y) con 1 <= x, y <= n y x < y.
paresHasta :: Int -> [(Int, Int)]
paresHasta n = [(x,y) | x <- [1..n], y <- [x+1..n]]

-- Dado un entero, devuelve ternas pitagóricas con lados hasta n.
pitagoricas :: Int -> [(Int, Int, Int)]
pitagoricas n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- Dado un entero, devuelve cuádruplas (a,b,c,d) tales que a+b = c+d, con 1 <= a,b,c,d <= n.
sumasIguales :: Int -> [(Int, Int, Int, Int)]
sumasIguales n = [(a,b,c,d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], a + b == c + d]


--------------------------------------------------
-- Nivel 3 – Con conteo, índices y manipulación.
-- Dada una lista, le agrega el índice a cada elemento.
conIndices :: [a] -> [(Int, a)]
conIndices xs = [(i,x) | (i,x) <- zip [0..] xs]

-- Dado un elemento y una lista, devuelve cuantas veces aparece el elemento en la lista.
cuenta :: Eq a => a -> [a] -> Int
cuenta n xs = 
    let matches = [x | x <- xs, x == n]
    in length matches

-- Dada una lista, devuelve los elementos que aparecen más de una vez, sin repeticiones.
repetidos :: Eq a => [a] -> [a]
repetidos xs = [x | (x,i) <- zip xs [0..] , elem x (take i xs)]


--------------------------------------------------
-- Nivel 4 – Juegos con condiciones.
-- Dada una lista, devuelve los números de la lista que son capicúas (igual al revés).
capicuas :: [Int] -> [Int]
capicuas xs = [x | x <- xs, show x == reverse (show x)]

-- Dada una lista, filtra la lista dejando solo elementos que no están seguidos por su consecutivo.
noConsecutivos :: [Int] -> [Int]
noConsecutivos xs = [x | (x,y) <- zip xs (drop 1 xs), y /= x+1]

-- Dado un entero n, devuelve todos los pares (x,y) entre 1 y n tal que x + y == n.
paresConSuma :: Int -> [(Int, Int)]
paresConSuma n = [(x,y) | x <- [1..n], y <- [1..n], x + y == n]

