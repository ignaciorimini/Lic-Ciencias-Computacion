-- Implementación de Leftist heaps.
-- El rango es el camino más corto desde la raíz hasta una hoja por el subárbol derecho.
type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving Show

-- Función que devuelve el rango de un heap.
rank :: Heap a -> Rank
rank E = 0
rank (N r _ _ _) = r

-- Función que mergea dos heaps O(lg n) (es la operación más importante de heaps).
--  Fusionamos siempre con el subárbol derecho porque en leftist heaps el derecho es más liviano.
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = 
    if x <= y   then makeH x a1 (merge b1 h2)
                else makeH y a2 (merge h1 b2)
    where
        -- Después de fusionar, nos aseguramos de que el hijo izquierdo (a) tenga mayor rank que el derecho (b).
        makeH x a b = if rank a >= rank b
            then N (rank b + 1) x a b
            else N (rank a + 1) x b a

-- Función para insertar un elemento en un heap O(lg n).
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (N 1 x E E) h

-- Función que devuelve el mínimo de un heap O(1).
findMin :: Ord a => Heap a -> a
findMin (N _ x a b) = x

-- Función que borra el elemento mínimo de un heap O(lg n).
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (N _ x a b) = merge a b

-- Ejemplos y casos de prueba.
h1 = N 1 2 E E -- Heap de rango 1 con unicamente el elemento 2.
h2 = N 1 5 E E