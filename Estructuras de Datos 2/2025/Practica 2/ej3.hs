-- Dar al menos dos ejemplos de funciones que tengan el tipo indicado en cada caso.

import Data.Char

-- Recibe una función numérica y se la aplica al valor 5.
-- (Int -> Int) -> Int
f1 :: (Int -> Int) -> Int
f1 f = f 5
-- Probar f1 (+2) -> Devuelve 7.

-- Recibe un entero y devuelve una función que suma tal entero a otro entero dado.
-- Int -> (Int -> Int)
f2 :: Int -> (Int -> Int)
f2 x = (+x)
-- Probar (f2 5) 5 -> Deuvelve 10.

-- Recibe una función y la compone con la función (*2).
-- (Int -> Int) -> (Int -> Int)
f3 :: (Int -> Int) -> (Int -> Int)
f3 f = (*2).f
-- Probar (f3 (+2)) 4 -> Devuelve (4+2)*2 = 12.

-- Recibe un número y devuelve True si es mayor a 0, False en caso contrario.
-- Int -> Bool
f4 :: Int -> Bool
f4 x = x > 0
-- Probar f4 (-1) -> Devuelve False.

-- Recibe un valor de verdad y devuelve la función AND con dicho valor de verdad en un argumento.
-- Bool -> (Bool -> Bool)
f5 :: Bool -> (Bool -> Bool)
f5 x = (&& x)
-- Probar (f5 True) True -> Devuelve True.

-- Recibe una tupla de un entero y un caracter y devuelve True si el codigo ASCII del caracter es el número de la tupla, False en caso contrario.
-- (Int, Char) -> Bool
f6 :: (Int, Char) -> Bool
f6 (n,c) = ord c == n
-- Probar f6 (65, 'A') -> Devuelve True.

-- Recibe una dupla de enteros y los suma entre sí.
-- (Int, Int) -> Int
f7 :: (Int, Int) -> Int
f7 (x,y) = x + y
-- Probar f7 (2,1) -> Devuelve 3.

-- Recibe un entero y devuelve una dupla con el entero dado y su doble.
-- Int -> (Int, Int)
f8 :: Int -> (Int, Int)
f8 x = (x, 2*x)
-- Probar f8 4 -> Devuelve (4, 8).

-- Función que siempre devuelve True.
-- a -> Bool
f9 :: a -> Bool
f9 _ = True

-- Función identidad.
-- a -> a
f10 :: a -> a
f10 x = x