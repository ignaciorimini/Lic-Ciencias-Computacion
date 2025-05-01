{-
La definicion de member dada en teoria (la cual determina si un elemento esta en un bst) realiza en el peor caso 2 ∗ d comparaciones, donde d es la altura del arbol. 

Dar una definicion de member que realice a lo sumo d + 1 comparaciones. Para ello definir member en terminos de una funcion auxiliar que tenga como parametro el elemento candidato, el cual puede ser igual al elemento que se desea buscar (por ejemplo, el ultimo elemento para el cual la comparacion de a 6 b retornó True) y que chequee que los elementos son iguales sólo cuando llega a una hoja del arbol.
-}

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

member :: Ord a => a -> BST a -> Bool
member val Hoja = False
member val arbol = aux val arbol val

aux :: Ord a => a -> BST a -> a -> Bool
aux val Hoja candidato = val == candidato
aux val (Nodo izq x der) candidato
    | val <= x  = aux val izq x
    | otherwise = aux val der candidato

