data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

--------------------------------------------------
-- a) Función nth.
-- Devuelve el n-ésimo elemento de una secuencia.
nth :: BTree a -> Int -> a
nth Empty _ = error "No elements in emtpy tree."
nth (Node size izq val der) i
    | i < 0 = error "No element in that position."
    | i >= size = error "No element in that position."
    | i == leftSize = val
    | i < leftSize = nth izq i
    | otherwise = nth der (i - leftSize - 1)
    where
        leftSize = getSize izq


-- Devuelve el tamaño de un árbol dado.
getSize :: BTree a -> Int
getSize Empty = 0
getSize (Node size _ _ _) = size


--------------------------------------------------
-- b) Función cons.
-- Inserta un elemento al comienzo de la secuencia.
cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node size izq val der) = rebalance (Node (size + 1) (cons x izq) val der)


-- Función que rebalancea un árbol dado.
rebalance :: BTree a -> BTree a
rebalance Empty = Empty
rebalance t@(Node size izq val der)
    | altura izq > altura der + 1 = rebalance (rotacionDer t)
    | altura der > altura izq + 1 = rebalance (rotacionIzq t)
    | otherwise = t


-- Función que devuelve la altura de un árbol.
altura :: BTree a -> Int
altura Empty = 0
altura (Node size izq val der) = 1 + max (altura izq) (altura der)


-- Función que efectua una rotación hacia la derecha del árbol.
rotacionDer :: BTree a -> BTree a
rotacionDer (Node size (Node sl ll vl rl) val der) = (Node size ll vl (Node (1 + getSize rl + getSize der) rl val der))
rotacionDer t = t


-- Función que efectua una rotación hacia la izquierda del árbol.
rotacionIzq :: BTree a -> BTree a
rotacionIzq (Node size izq val (Node sr lr vr rr)) = (Node size (Node (1 + getSize izq + getSize lr) izq val lr) vr rr)
rotacionIzq t = t


--------------------------------------------------
-- c) Función tabulate.
-- Dada una función f y un entero n, devuelve una secuencia de tamaño n, donde cada elemento de la secuencia es el resultado de aplicar f al índice del elemento. <f 0, f 1, f 2, ... f n>
-- tabulate :: (Int -> a) -> Int -> BTree a
-- tabulate f 0 = Empty
-- tabulate f 1 = (Node 1 Empty (f 0) Empty)
-- tabulate f n = tabulate' f n 0

-- tabulate' :: (Int -> a) -> Int -> Int -> BTree a
-- tabulate' f n i
--     | i < n = cons (f i) (tabulate' f n (i+1))
--     | otherwise = Empty


tabulate :: (Int -> a) -> Int -> BTree a
tabulate _ 0 = Empty
tabulate f n = tabulate' f 0 (n - 1)

-- Función auxiliar tabulate'
-- lo y hi representa un rango de índices. al dividir siempre por 2, mantenemos el árbol balanceado.
tabulate' :: (Int -> a) -> Int -> Int -> BTree a
tabulate' f lo hi
    | lo > hi = Empty
    | otherwise = 
        let mid = (lo + hi) `div` 2
            ((izq, der), val) = (tabulate' f lo (mid - 1)) || (tabulate' f (mid + 1) hi) || (f mid)
        in Node (hi - lo + 1) izq val der


--------------------------------------------------
-- d) Función map.
-- Dada una función f y una secuencia s, devuelve el resultado de aplicar f sobre cada elemento de s.
map' :: (a -> b) -> BTree a -> BTree b
map' f Empty = Empty
map' f (Node size izq val der) = 
    let (izq', der') = (map' f izq || map' f der)
    in (Node size izq' (f val) der')


--------------------------------------------------
-- e) Función take.
-- Dados un entero n y una secuencia s, devuelve los primeros n elementos de s.
take' :: Int -> BTree a -> BTree a
take' n t@(Node size izq val der)
    | n <= 0 = Empty
    | n >= size = t
    | n <= leftSize = take' n izq
    | otherwise = (Node n izq val (take' (n - leftSize - 1) der))
    where
        leftSize = getSize izq


--------------------------------------------------
-- e) Función drop.
-- Dados un entero n y una secuencia s, devuelve la secuencia s sin los primeros n elementos.
drop' :: Int -> BTree a -> BTree a
drop' n t@(Node size izq val der)
    | n <= 0 = t
    | n >= size = Empty
    | n <= leftSize = (Node n (drop' n izq) val der)
    | otherwise = drop' (n - leftSize - 1) der
    where
        leftSize = getSize izq