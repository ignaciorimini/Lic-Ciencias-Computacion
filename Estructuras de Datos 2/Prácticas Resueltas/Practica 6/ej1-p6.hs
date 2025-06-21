{-
Definir las siguientes funciones, correspondientes a la interfaz de Secuencias, implementando las secuencias con árboles binarios, definidos con el siguiente tipo de datos.

data BTree a = Empty | Node Int (BTree a) a (BTree a)

donde se almacenan los tamaños de los árboles en los nodos. Suponer que el recorrido inorder del árbol da el orden de los elementos de la secuencia. Calcular el trabajo y la profundidad de cada una. Resolver la recurrencia
y expresar la solución en términos del orden O.

<3,6,8,11,13> -> Node 5 (Node 2 Empty 3 (Node 1 Empty 6 Empty)) 8 (Node 2 (Node 1 Empty 11 Empty) 13 Empty)
-}

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- Devuelve el tamaño de una secuencia.
size :: BTree a -> Int
size Empty = 0
size (Node s _ _ _) = s

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node _ l a r) = inorder l ++ [a] ++ inorder r

-- Calcula el n-ésimo elemento de una secuencia.
nth :: BTree a -> Int -> a
nth Empty _ = error "Error: índice fuera de rango"
nth (Node s l a r) i
    | i < 0 = error "Error: índice fuera de rango"
    | i > s = error "Error: índice fuera de rango"
    | i < leftSize = nth l i
    | i > leftSize = nth r (i - leftSize - 1)
    | otherwise = a
    where
        leftSize = size l

-- Inserta un elemento al comienzo de la secuencia.
cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node s l a r) = Node s (cons x l) a r

-- Dada una función f y un entero n devuelve una secuencia de tamaño n, donde cada elemento de la secuencia es el resultado de aplicar f al índice del elemento.
tabulate :: (Int -> a) -> Int -> BTree a
tabulate f n = go 0 n
    where
        go i 0 = Empty
        go i n = 
            let mid = div n 2
                x = f (i + mid)
                (left, right) = (go i mid, go (i + mid + 1) (n - mid - 1))
            in Node n left x right

-- Dada una función f y una secuencia s, devuelve el resultado de aplicar f sobre cada elemento de s.
mapT :: (a -> b) -> BTree a -> BTree b
mapT f Empty = Empty
mapT f (Node s l a r) = 
    let (aplicarIzq, aplicarDer) = (mapT f l, mapT f r)
    in Node s aplicarIzq (f a) aplicarDer

-- Dados un entero n y una secuencia s, devuelve los primeros n elementos de s.
takeT :: Int -> BTree a -> BTree a
takeT n Empty = Empty
takeT n t@(Node s l a r)
    | n <= 0 = Empty
    | n >= s = t
    | n < leftSize = takeT n l
    | n > leftSize = Node n l a (takeT (n - leftSize - 1) r)
    | otherwise = l
    where
        leftSize = size l
        rightSize = size r

-- Dados un entero n y una secuencia s, devuelve la secuencia s sin los primeros n elementos.
dropT :: Int -> BTree a -> BTree a
dropT n Empty = Empty
dropT n t@(Node s l a r)
    | n <= 0 = t
    | n >= s = Empty
    | n < leftSize = Node (s - n) (dropT n l) a r
    | n > leftSize = dropT (n - leftSize - 1) r
    | otherwise = Node (s - n) Empty a r
    where
        leftSize = size l

-- Ejemplos.
secuencia = Node 5 (Node 2 Empty 3 (Node 1 Empty 6 Empty)) 8 (Node 2 (Node 1 Empty 11 Empty) 13 Empty)

{-
Calcular trabajo y profundidad de cada una. Resolver la recurrencia y expresar la solución en términos del orden O.

...
-}