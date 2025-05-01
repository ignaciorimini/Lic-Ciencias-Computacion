{-
RED BLACK TREES
Los red-black trees son árboles binarios de búsqueda con nodos "coloreados" rojos o negros, que cumplen las siguientes invariantes para mantener al árbol balanceado:
1) Ningún nodo rojo tiene hijos rojos.
2) Todos los caminos de la raíz a una hoja tienen el mismo número de nodos nergos (altura negra).

Definición:
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

Observaciones:
- En un RBT, el camino más largo es a lo sumo el doble que el camino más corto.
- En un RBT, la altura es O(log n) (osea, es un árbol balanceado).
-}

data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

--------------------------------
-- Función que determina si un elemento pertenece al RBT es igual que para BSTs, simplemente ignoramos el color.
member :: Ord a => a -> RBT a -> Bool
member _ E = False
member val (T _ izq x der)
    | val == x  = True
    | val < x   = member val izq
    | val > x   = member val der


--------------------------------
-- Inserciones en RBTs
insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where   ins x E = T R E x E
            ins x (T c izq y der)   | x < y     = balance c (ins x izq) y der
                                    | x > y     = balance c izq y (ins x der)
                                    | otherwise = T c izq y der
            makeBlack E = E
            makeBlack (T _ izq x der) = T B izq x der

-- Notar que el nodo nuevo se inserta como un nodo rojo, por lo ue se mantiene la altura negra. 
-- Pero se puede violar el invariante 1, por lo que hay que rebalancear. Luego del rebalanceo puede quedar una raíz roja, por lo que se colorea de negro la raíz.

-- Luego de insertar el nuevo nodo rojo hay a lo sumo una única violación del invariante 1, que ocurre cuando el padre es rojo. Por lo tanto la violación siempre ocurre en un camino B-R-R.

-- La función balance va arreglando y propagando hacia arriba esta violación. La (única) violacion, puede aparecer en cuatro configuraciones. En todos los casos la solución es la misma: reescribir el nodo como un padre rojo con dos hijos negros.

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c i a r = T c i a r

-- Wbalance es O(1) pues el árbol está balanceado.
-- Winsert es O(log n)
