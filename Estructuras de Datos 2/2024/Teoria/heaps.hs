{-
HEAPS
Los heaps (montículos) son árboles que permiten un acceso eficiente al mínimo elemento.

Mantienen la invariante de que todo nodo es menor a todos los valores de sus hijos. Por lo tanto, el mínimo siempre está en la raíz.

Un heap debe soportar eficientemente las operaciones:
insert :: Ord a => a -> Heap a -> Heap a
findMin :: Ord a => Heap a -> a
deleteMin :: Ord a => Heap a -> Heap a

-------------------
LEFTIST HEAPS
Los leftist heaps son heaps que tienen la propiedad de estructura izquierdista, esto es que para cada nodo, la longitud del camino más corto desde el nodo hasta un nodo hoja en el subárbol izquierdo es al menos tan largo como el camino más corto desde el nodo hasta un nodo hoja en el subárbol derecho. En otras palabras, el camino más corto está en el lado izquierdo del árbol.

El rango de un heap es entonces, la longitud de la espina derecha (el camino hacia la derecha hasta un nodo vacío).

Invariante Leftist: el rango de cualquier hijo izquierdo es mayor o igual que el de su hermano de la derecha.

Consecuencias:
- La espina derecha es el camino más corto a un nodo vacío.
- La longitud de la espina derecha es a lo sumo log(n+1).
- Los elementos de la espina derecha están ordenados (como consecuencia de la invariante de heap).
-}