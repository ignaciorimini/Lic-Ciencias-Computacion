module Test where
-- Ejecutar ghci listasComprension para usar el programa con interprete.

concatenar :: [[a]] -> [a]
concatenar xss = [x | xs <- xss, x <- xs]

-- concatenar [[1,2,3],[4,5,6]] devuelve [1,2,3,4,5,6]
-- concatenar [['a','b','c'],['d','e','f']] devuelve "abcdef"
-- concatenar ["hola", "mundo"] devuelve "holamundo"


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

esPrimo :: Int -> Bool
esPrimo n = factors n == [1,n]

primos :: Int -> [Int]
primos n = [x | x <- [2..n], esPrimo x]


-- factors 4 devuelve [1,2,4]
-- factors 12 devuelve [1,2,3,4,6,12]

-- esPrimo 3 devuelve True
-- esPrimo 4 devuelve False
-- primos 10 devuelve [2,3,5,7]


esMinuscula :: Char -> Bool
esMinuscula x = x >= 'a' && x <= 'z'

cantMinusc :: String -> Int
cantMinusc xs = length[x | x <- xs, esMinuscula x]

-- cantMinusc "Hola" devuelve 3