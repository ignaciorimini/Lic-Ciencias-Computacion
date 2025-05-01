{-
Si un arbol binario es dado como un nodo con dos subarboles identicos se puede aplicar la tecnica sharing para que los subarboles sean representados por el mismo arbol. 

Definir las siguientes funciones de manera que se puedan compartir la mayor cantidad posible de elementos de los arboles creados:

a) completo :: a → Int → Tree a
Tal que dado un valor x de tipo a y un entero d, crea un arbol binario completo de altura d con el valor x en cada nodo.

b) balanceado :: a → Int → Tree a
Tal que dado un valor x de tipo a y un entero n, crea un arbol binario balanceado de tamaño n, con el valor x en cada nodo.
-}

data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show

completo :: a -> Int -> Tree a
completo _ 0 = Hoja
completo x 1 = Nodo Hoja x Hoja
completo x n = Nodo (completo x (n-1)) x (completo x (n-1))

balanceado :: a -> Int -> Tree a
balanceado _ 0 = Hoja
balanceado x n  | n <= 0            = Hoja
                | (mod n 2) == 0    = 
                | otherwise         = 
