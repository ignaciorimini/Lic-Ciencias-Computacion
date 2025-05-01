-- Ejemplo: heap = Root 3 [Root 5 [Root 10 []], Root 6 [Root 8 []]]
data PHeaps a = Empty | Root a [PHeaps a] deriving Show

-- Función que determina si un elemento de tipo PHeaps es un heap.
isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root x subheaps) = all (checkRoot x) subheaps && all isPHeap subheaps
    where
        all :: (a -> Bool) -> [a] -> Bool
        all p [] = True
        all p (x:xs) = p x && all p xs

        checkRoot :: Ord a => a -> PHeaps a -> Bool
        checkRoot _ Empty = True
        checkRoot parentVal (Root childVal _) = parentVal <= childVal


-- Función que une dos pairing heaps. Para ello, compara las raíces de ambos árboles y elige la menor como raíz del  nuevo heap, luego, agrega el árbol con mayor raíz como hijo de éste.
merge :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(Root x subHeaps1) h2@(Root y subHeaps2)
    | x <= y = Root x (h2 : subHeaps1)
    | otherwise = Root y (h1 : subHeaps2)


-- Función que inserta un elemento en un pairing heap.
insert :: Ord a => PHeaps a -> a -> PHeaps a
insert Empty x = Root x []
insert h x = merge (Root x []) h


-- Dada una lista de pairing heaps, contruye otro con los elementos del mismo.
concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [] = Empty
concatHeaps (h:hs) = foldr merge Empty (h:hs)

