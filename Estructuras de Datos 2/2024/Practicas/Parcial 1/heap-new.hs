{-
PROPIEDADES DEL LEFTIST HEAP
- Heap property: en un min-heap, para cualquier nodo x, el valor de x es menor o igual que los valores de sus hijos.
- Leftist property: para cada nodo, la distancia a la hoja más cercana en el subárbol derecho es menor o igual que la distancia a la hoja más cercana en el subárbol izquierdo. Esta distancia se conoce como RANK del nodo (distancia a la hoja más cercana).

Esta propiedad asegura que el subárbol derecho es más corto o igual en altura que el subárbol izquierdo, facilitando la fusión eficiente. La longitud del subárbol derecho es a lo sumo log(n + 1).
-}

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

-- Función que dado un heap, devuelve su rango.
-- rank = O(1)
rank :: Heap a -> Rank
rank E = 0
rank (N r _ _ _) = r


-- Función que dados dos heaps, los fusiona en uno solo.
-- Como rank, makeH = O(1) y el subárbol derecho es a lo sumo logarítmico, merge = O(lg n).
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
    if x <= y
        then makeH x a1 (merge b1 h2)
        else makeH y a2 (merge h1 b2)
    where
        -- makeH = O(1)
        makeH :: Ord a => a -> Heap a -> Heap a -> Heap a
        makeH x a b =
            if rank a >= rank b
                then N (rank b + 1) x a b
                else N (rank a + 1) x b a


-- Función que dado un elemento y un heap, inserta el elemento en el heap.
-- insert = O(lg n)
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (N 1 x E E) h


-- Función que dado un heap, devuelve el mínimo elemento del mismo.
-- findMin = O(1)
findMin :: Ord a => Heap a -> a
findMin (N _ x _ _) = x


-- Función que dado un heap, devuelve un heap quitando el mínimo.
-- deleteMin = O(lg n)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (N _ x a b) = merge a b