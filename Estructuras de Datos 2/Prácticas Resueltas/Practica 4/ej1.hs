data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

-- Función que dado un valor x de tipo a, y un entero d, crea un árbol binario completo de altura d con el valor x en cada nodo.
completo :: a -> Int -> Tree a
completo _ 0 = Empty
completo x d = 
    let ramaNueva = completo x (d-1)
    in Node ramaNueva x ramaNueva

-- Función que dado un valor x de tipo a y un entero n, crea un árbol binario balanceado de tamaño n, con el valor x en cada nodo.
balanceado :: a -> Int -> Tree a
balanceado _ 0 = Empty
balanceado x n =
    let (q,r) = divMod (n-1) 2
        left = balanceado x (q+r)
        right = balanceado x q
    in Node left x right

-- divMod es una función de Haskell que toma dos números y devuelve una tupla con:
-- q: El cociente de la división entera (es decir, la parte entera del resultado), y
-- r: El resto de esa división.
