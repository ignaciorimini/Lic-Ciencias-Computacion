{-
Agregar a la siguiente definición del TAD de árboles balanceados una especificación algebraica para las operaciones SIZE y EXPOSE.

tad BalT (A:Ordered Set) where
    import maybe
    empty : BalT A
    join : BalT -> Maybe A -> BalT -> BalT
    size : BalT A -> N
    expose : BalT A -> Maybe (BalT A, A, BalT A)

- La operación join toma un árbol L, un elemento opcional, y un árbol R. Si L y R son árboles de búsqueda balanceados tales que todos los elementos de L sean menores que todos los elementos de R y el elemento opcional es más grande que los de L y menor que los de R, entonces join crea un nuevo árbol de búsqueda balanceado.

- Las operaciones empty y size son obvias.

- La operación expose toma un árbol T y nos da Nothing si el árbol está vacío, y en otro caso nos devuelve un árbol izquierdo, un elemento raíz, y un árbol derecho de un árbol de búsqueda que contiene todos los elementos de T.

Notar que join no es simplemente un constructor sino que tiene que realizar cierto trabajo para devolver un árbol balanceado. Debido a esto es conveniente especificar expose por casos sobre su resultado.

---------------------------------------
ESPECIFICACIÓN ALGEBRAICA

join empty Nothing empty = empty
join L (Just x) empty = insert x L
join empty (Just x) R = insert x R
join L Nothing R = merge L R
join L (Just x) R = balance L x R

size empty = 0
size (join L Nothing R) = size L + size R
size (join L (Just x) R) = 1 + size L + size R

expose empty = Nothing
expose (join empty (Just x) empty) = Just (empty, x, empty)
expose (join L (Just x) R) = Just (L, x, R)
expose (join L Nothing R) = expose (merge L R)


-}