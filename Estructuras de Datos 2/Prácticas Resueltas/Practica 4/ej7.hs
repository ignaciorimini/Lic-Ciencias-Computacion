data PHeaps a = Empty | Root a [PHeaps a] deriving Show

-- Función que determina si un árbol es un pairing heap, es decir, cumple con el invariante de heap.
isPHeap :: Ord a => PHeaps a -> Bool
isPheap Empty = True
isPHeap (Root x [Empty]) = True
isPHeap (Root x hs) = isRootLeqChild x hs && areChildrenPH hs
  where
    isRootLeqChild :: Ord a => a -> [PHeaps a] -> Bool
    isRootLeqChild x [] = True
    isRootLeqChild x (Empty:hs) = isRootLeqChild x hs
    isRootLeqChild x (Root y h:hs) = (x <= y) && (isRootLeqChild x hs)

    areChildrenPH :: Ord a => [PHeaps a] -> Bool
    areChildrenPH [] = True
    -- areChildrenPH (Empty:hs) = areChildrenPH hs
    -- areChildrenPH (Root x h:hs) = isPHeap (Root x h) && areChildrenPH hs
    areChildrenPH (t:ts) = isPHeap t && areChildrenPH ts

-- Función que une dos pairing heaps. Para ello, compara las raíces de ambos árboles y elije la menor como raíz del nuevo heap. Luego, agrega el árbol con mayor raíz como hijo de este.
merge :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(Root x1 (y:ys)) h2@(Root x2 (z:zs)) =
  if x1 <= x2
    then case y of
          Empty -> Root x1 [h2]
          Root _ _-> Root x1 ([h2] ++ (y:ys))
    else case z of
          Empty -> Root x2 [h1]
          Root _ _ -> Root x2 ([h1] ++ (z:zs))

-- Función que inserta un elemento en un pairing heap.
insert :: Ord a => PHeaps a -> a -> PHeaps a
insert h x = merge h (Root x [Empty])

-- Función que dada una lista de pairing heaps construya otro con los elementos del mismo.
concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [] = Empty
concatHeaps (x:xs) = merge x (concatHeaps xs)

-- Función que dado un pairing heap devuelve si el árbol no es vacío, un par con el menor elemento y un pairing heap sin este elemento; o Nothing en otro caso.
delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root x ys) = Just (x, concatHeaps ys) 

-- Ejemplos y casos de prueba.
pheap1 = Root 1 [Root 2 [Root 4 [Empty]], Root 3 [Empty], Root 4 [Root 6 [Empty], Root 7 [Empty]]]
pheap2 = Root 2 [Root 3 [Empty], Root 4 [Empty], Root 5 [Empty]]
pheap3 = Root 6 [Root 7 [Empty],Root 8 [Empty]]