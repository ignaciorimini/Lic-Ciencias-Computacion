module Test where
-- Dar al menos dos ejemplos de funciones que tengan el tipo indicado en cada caso:

--------------------------
-- 1) (Int -> Int) -> Int
-- Funcion que toma una función como argumento y se la aplica al número 5.
-- g(f) = f(5)
funcion1a :: (Int -> Int) -> Int
funcion1a f = f 5
-- funcion1a (+3) -> (+3) 5 = 8

-- Funcion que toma una función como argumento y se la aplica al número 10 y luego duplica el resultado.
-- g(f) = 2 * f(10)
funcion1b :: (Int -> Int) -> Int
funcion1b f = 2 * f 10
-- funcion1b (+ 2) = 2 * (+2) 10 = 2 * 12 = 24


--------------------------
-- 2) Int -> (Int -> Int)
-- Funcion que toma un entero y devuelve la función que le suma tal entero a otro.
-- g(x) = f(y) = y + x
funcion2a :: Int -> (Int -> Int)
funcion2a x = (+ x)
-- funcion2a 5 2 -> (+5) 2 = 7

-- Función que toma un entero y devuelve la función que multiplica cualquier número por el entero dado.
-- g(x) = f(y) = y * x
funcion2b :: Int -> (Int -> Int)
funcion2b x = (* x)
-- funcion2b 3 4 = (*3) 4 = 12


--------------------------
-- 3) (Int -> Int) -> (Int -> Int)
-- Función que recibe una función como argumento y devuelve la composición de funciones f con la función incremento. Aplica primero f y luego le suma 1 al resultado.
-- g(f) = (+1) . f(x)
funcion3a :: (Int -> Int) -> (Int -> Int)
funcion3a f = f . (+ 1)
--  funcion3a (+2) 3 -> (+1) ((+2) 3) = (+1) 5 = 6

-- Función que recibe una función como argumento y devuelve la composición de esa función con la función doble y luego con la función incremento.
-- Le suma 1 a x, luego aplica f y finalmente multiplica por 2
-- g(f)= ((*2) . f) . (+1)
funcion3b :: (Int -> Int) -> (Int -> Int)
funcion3b f = (*2) . f . (+1)
-- funcion3b (+2) 4 -> (*2(+2(+1 4))) = (*2(+2 5)) = (*2 7) = 14


--------------------------
-- 4) Int -> Bool
-- Funcion que toma un argumento de tipo Int y devuelve True si el argumento es 3, False en caso contrario.
funcion4a :: Int -> Bool
funcion4a x = x == 3
-- funcion4a 5 -> False
-- funcion4a 3 -> True

-- Función que toma un entero y devuelve True si es par, False en caso contrario.
funcion4b :: Int -> Bool
funcion4b x = mod x 2 == 0
-- funcion4b 10 -> True
-- funcion4b 3 -> False


--------------------------
-- 5) Bool -> (Bool -> Bool)
-- Función que toma un booleano como argumento. 
-- Si el argumento es True devuelve la funcion f(x) = x || False
-- Si el argumento es False devuelve la función f(x) = x && True
-- En resumen, esta función devuelve True cuando el segundo argumento es True.
funcion5a :: Bool -> (Bool -> Bool)
funcion5a x 
    | x         = (|| False)
    | otherwise = (&& True)
-- funcion5a True False -> False || False -> False
-- funcion5a True True -> True || False -> True
-- funcion5a False True -> True && True -> True
-- funcion5a False False -> False && False -> False

-- Función que devuelve True solo si los dos argumentos son True. Es la función AND.
-- x && (False || y)
funcion5b :: Bool -> (Bool -> Bool)
funcion5b x = (&& x) . (|| False)
-- funcion5b True False -> False
-- funcion5b True True -> True

-- Función que devuelve True solo si los dos argumentos son False.
-- not (x || (False || y))
funcion5c :: Bool -> (Bool -> Bool)
funcion5c x = not . (|| x) . (|| False)


--------------------------
-- 6) (Int, Char) -> Bool
-- Función que toma una tupla con un entero y un caracter y devuelve True si el entero especifica la posición del caracter en el alfabeto.
-- Por ejemplo, (0, 'a') devuelve True pues en la lista ['a', 'b', ... 'z'] a está en el primer elemento. Sin embargo (10, 'a') devuelve True pues a no está en la posición 10 de la lista.
funcion6a :: (Int, Char) -> Bool
funcion6a (a, b) = ['a'..'z'] !! a == b
-- funcion6a (0, 'a') -> True
-- funcion6a (25, 'z') -> True

-- Función que toma una tupla con un entero y un caracter y devuelve True si el caracter tiene código ASCII mayor que el caracter de posición alfabeto[a].
funcion6b :: (Int, Char) -> Bool
funcion6b (a, b) = b >= ['a'..'z'] !! a
-- funcion6b (0, 'e') -> True pues 'e' >= 'a'
-- funcion6b (3, 'b') -> False pues 'b' < 'd'


--------------------------
-- 7) (Int, Int) -> Int
-- Función que devuelve la primer componente de la tupla pasada como argumento.
funcion7a :: (Int, Int) -> Int
funcion7a (x, y) = x
-- funcion7a (1, 2) -> 1
-- funcion7a (47, 15) -> 47

-- Función que retorna el mayor número de la tupla pasada como argumento.
funcion7b :: (Int, Int) -> Int
funcion7b (x, y)
    | x >= y    = x
    | otherwise = y
-- funcion7b (60, 54) -> 60
-- funcion7b (1, 4) -> 4


--------------------------
-- 8) Int -> (Int, Int)
funcion8a :: Int -> (Int, Int)
funcion8a x = (x, 0)

funcion8b :: Int -> (Int, Int)
funcion8b x = (x, x*2)