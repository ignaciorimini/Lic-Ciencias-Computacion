{-
La definicion de member dada en teoria (la cual determina si un elemento esta en un bst) realiza en el peor caso 2 ∗ d comparaciones, donde d es la altura del arbol. 

Dar una definicion de member que realice a lo sumo d + 1 comparaciones. Para ello definir member en terminos de una funcion auxiliar que tenga como parametro el elemento candidato, el cual puede ser igual al elemento que se desea buscar (por ejemplo, el ultimo elemento para el cual la comparacion de a 6 b retornó True) y que chequee que los elementos son iguales sólo cuando llega a una hoja del arbol.
-}

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

-- Función que determina si un elemento pertenece a un bst con d+1 comparaciones como mucho.
member :: Ord a => a -> BST a -> Bool
member _ Hoja = False
member x t = aux x t x

-- Recibe un elemento, un árbol y un elemento candidato. Solo compara el elemento con el elemento candidato cuando se llega a una hoja.
aux :: Ord a => a -> BST a -> a -> Bool
aux x Hoja candidato = x == candidato
aux x (Nodo l a r) candidato
    | x <= a = aux x l a
    | otherwise = aux x r candidato

-- Ejemplos y casos de prueba.
bst1 = Nodo (Nodo Hoja 4 Hoja) 6 (Nodo (Nodo Hoja 8 Hoja) 10 (Nodo Hoja 12 Hoja))