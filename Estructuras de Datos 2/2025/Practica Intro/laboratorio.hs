module Lab01 where

import Data.List

import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a) Problema: not ya está definido en el preludio. Definimos una nueva función myNot.
myNot b = case b of True -> False
                    False -> True

-- b) Problema: 'in' es una palabra resrvada en Haskell. Le cambiamos el nombre a la función.
myIn [x]         =  []
myIn (x:xs)      =  x : myIn xs
myIn []          =  error "empty list"

-- c) Problema: en Haskell las funciones deben comenzar con minúscula (en mayúsculas van las clases). Solución: cambiar el nombre de la función a length.
myLength []        =  0
myLength (_:l)     =  1 + myLength l

-- d) Problema: el operador cons (:) toma por argumento un elemento y una lista de mismo tipo de argumento. Es decir:
-- (:) :: a -> [a] -> [a]
-- En este caso se está utilizando el operador (:) entre dos números, lo cual es un error.
list123 = 1:(2:(3:[]))

-- e) Problema: declaración de la función ++! infija. Solución: definirla de manera prefija y con paréntesis.
(++!) [] ys = ys
(++!) (x:xs) ys = x : ((++!) xs ys)

-- f) Esta función suma algebraicamente el valor del primer argumento a toda la lista sin el primer elemento. El problema es que no estaban bien aplicado los paréntesis.
addToTail x xs = map (+x) (tail xs)

-- g) Eliminar el punto y colocar paréntesis. La función sort es importada de Data.List
listmin xs = head (sort xs)

-- h) Aplica la función f a todo elemento de la lista. Solución: colocar bien los paréntesis.
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : (smap f xs)


-- ---------------------------------------------------------
-- 2. Definir las siguientes funciones y determinar su tipo:

-- a) five, que dado cualquier valor, devuelve 5.
five :: a -> Int
five _ = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de aplicar la función al valor dado.
apply :: (a -> b) -> a -> b
apply f x = f x

-- c) ident, la función identidad.
ident :: a -> a
ident x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente.
first :: (a,b) -> a
first (x, _) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado.
derive :: Floating a => (a -> a) -> a -> a -> a
derive f h x = (f (x+h) - f x)/h
-- Probar definiendo cuadrado x = x^2 y llamar a la función derive cuadrado 0.01 4 = 8

-- f) sign, la función signo.
sign :: (Num a, Ord a) => a -> a
sign x  | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0

-- g) vabs, la función valor absoluto (usando sign y sin usarla).
vabs :: (Num a, Ord a) => a -> a
vabs x  | sign x == 1   = x
        | sign x == -1  = -x
        | otherwise = 0

vabs2 :: (Num a, Ord a) => a -> a
vabs2 x | x >= 0    = x
        | x < 0     = -x

-- h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero.
pot :: (Integral a, Num b) => a -> b -> b
pot n x = x^n

-- i) xor, el operador de disyunción exclusiva.
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- j) max3, que toma tres números enteros y devuelve el máximo entre ellos.
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas.
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


{- ---------------------------------------------------------
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

esBisiesto :: Int -> Bool
esBisiesto año
    | año `mod` 400 == 0    = True
    | año `mod` 100 == 0    = False
    | año `mod` 4 == 0      = True
    | otherwise             = False


{- -----------------------------------------------------------
4)
Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}
infixl 7 *$

(*$) :: Num a => [a] -> a -> [a]
ns *$ n = map (* n) ns

v = [1, 2, 3] *$ 2 *$ 4


-- ---------------------------------------------------------
-- 5) Definir las siguientes funciones usando listas por comprensión:

-- a) 'divisors', que dado un entero positivo 'x' devuelve la 
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors :: Int -> [Int]
divisors x
    | x > 0 = [d | d <- [1..x], x `mod` d == 0]
    | otherwise = []

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches x xs = [x | y <- xs, x == y]

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
-- '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
-- donde 0 <= a, b, c, d <= 'n'
cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

-- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
-- 'xs' sin elementos repetidos
unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]


{- ----------------------------------------------------------
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. 

La función zip mapea dos listas a una lista con los pares de elementos de mismo índice.
zip :: [a] -> [b] -> [(a,b)]
zip [1,2,3] ['a','b','c'] -> [(1,'a'), (2, 'b'), (3, 'c')]

La función sum recibe una lista de números y devuelve la suma de todos ellos.
sum [1,2,3,4] = 10

Ejemplo: 
scalarProduct [1, 2, 3] [4, 5, 6] -> (1*4 + 2*5 + 3*6) = 4 + 10 + 18 = 32
-}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]


----------------------------------------------------------
-- 7) Definir mediante recursión explícita
-- las siguientes funciones y escribir su tipo más general:

-- a) 'suma', que suma todos los elementos de una lista de números
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs)   | x = True
                | otherwise = alguno xs

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs)    | x == False = False
                | otherwise = todos xs

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales
codes :: String -> [Int]
codes [] = []
codes (x:xs) = ord x : codes xs

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado
restos :: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) n = (x `mod` n) : restos xs n

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes.
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs

-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda
orden :: [(Int,Int)] -> [(Int, Int)]
orden [] = []
orden ((x,y):xs)    | x < y*3   = (x,y) : orden xs
                    | otherwise = orden xs

-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | even x = x : pares xs
             | otherwise = pares xs

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)
letras :: [Char] -> [Char]
letras [] = []
letras (x:xs)
    | isAlpha x = x : letras xs
    | otherwise = letras xs

-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n'
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n
    | length x > n  = x : masDe xs n
    | otherwise     = masDe xs n