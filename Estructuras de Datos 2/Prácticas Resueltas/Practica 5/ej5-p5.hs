{-
5)
tad BalT (A:Ordered Set) where
    import Maybe
    empty : BalT A
    join : BalT A -> Maybe A -> BalT A -> BalT A
    size : BalT A -> N
    expose : BalT A -> Maybe (BalT A, A, BalT A)

• La operación join toma un árbol L, un elemento opcional, y un árbol R. Si L y R son árboles de búsqueda balanceados tales que todos los elementos de L sean menores que todos los elementos de R y el elemento opcional es más grande que los de L y menor que los de R, entonces join crea un nuevo árbol de búsqueda balaceado.
• Las operaciones empty y size son obvias.
• La operación expose toma un árbol T y nos da Nothing si el árbol está vacío, y en otro caso nos devuelve un árbol izquierdo, un elemento raíz, y un árbol derecho de un árbol de búsqueda que contiene todos los elementos de T.

___________________________________________________
a) Especificaciones algebraicas.

size empty = 0
size (join L Nothing R) = size L + size R
size (join L (Just x) R) = 1 + size L + size R 

expose empty = Nothing
expose (join empty (Just x) empty) = Just (empty, x, empty)
expose (join L (Just x) R) = Just (L, x, R)
expose (join L Nothing R) = expose (merge L R)
-}