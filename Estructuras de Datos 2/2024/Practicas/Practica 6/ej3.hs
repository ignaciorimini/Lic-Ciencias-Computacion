{-
Dados los diferentes valores de las acciones de YPF a lo largo del tiempo, se desea saber cual es la mejor ganancia que se puede obtener al comprar acciones un d´ıa y venderlas otro.

Definir una funcion mejorGanancia :: Tree Int → Int que calcule la mejor ganancia dada una secuencia de valores, utilizando el siguiente algoritmo:

1. Armar pares de la forma (compra, ventas), donde compra es el precio al cual se puede comprar una acción y ventas los distintos valores en que puede venderse.
2. Para cada par de la forma (compra, ventas) calcular las diferencias venta - compra, donde venta es un elemento de ventas.
3. Tomar el número máximo de las diferencias calculadas en el paso anterior.

Definir las siguientes funciones que implementan distintas partes del algoritmo y utilizarlas para definir mejorGanancia.
-}

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

-- FUNCIÓN SUFIJOS
-- Dado un árbol t, construye otro con los sufijos de cada elemento de t.
-- El sufijo de cada elemento es la lista que incluye al elemento actual y todos los elementos que le siguen en el árbol.
-- t = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)
-- sufijos t = Join (Join (Leaf (Join (Leaf 15) (Leaf 20))) (Leaf (Leaf 20))) (Leaf E)

sufijos :: Tree Int -> Tree (Tree Int)
sufijos t = sufijos' t E where
  sufijos' (Leaf _)   a = Leaf a
  sufijos' (Join l r) a = let (l', r') = if (a == E)
                                         then (sufijos' l r, sufijos' r E)
                                         else sufijos' l (Join r a) ||| sufijos' r a
                          in Join l' r'


zipT :: Tree a -> Tree b -> Tree (a,b)
zipT (Leaf x) (Leaf y) = Leaf (x,y)
zipT (Join l r) (Join l' r') = Join (zipT l l') (zipT r r')
zipT _ _ = E

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = zipT t (sufijos t)
