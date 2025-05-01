data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- Función principal splitAt
splitAt1 :: BTree a -> Int -> (BTree a, BTree a)
splitAt1 Empty _ = (Empty, Empty)
splitAt1 t@(Node size izq val der) i = splitAt' t i Empty


-- Función auxiliar splitAt'
splitAt' :: BTree a -> Int -> BTree a -> (BTree a, BTree a)
splitAt' Empty _ _ = (Empty, Empty)
splitAt' t@(Node size izq val der) i acc
    | i <= 0 = (acc, t)
    | i >= size = (combinar acc t, Empty)
    | i == leftSize = (combinar acc izq, Node (derSize + 1) Empty val der)
    | i < leftSize = let (newIzq, remIzq) = splitAt' izq i Empty
                     in (combinar acc newIzq, combinar remIzq (Node (derSize + 1) Empty val der))
    | otherwise = let (newDer, remDer) = splitAt' der (i - leftSize - 1) (Node (leftSize + 1) izq val Empty)
                  in (combinar acc (Node (leftSize + 1) izq val Empty), remDer)
    where
        leftSize = getSize izq
        derSize = getSize der


-- Calcula el tamaño de un árbol
getSize :: BTree a -> Int
getSize Empty = 0
getSize (Node size _ _ _) = size


-- Calcula la altura de un árbol
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l _ r) = 1 + max (altura l) (altura r)


-- Función para combinar dos árboles
combinar :: BTree a -> BTree a -> BTree a
combinar Empty t = t
combinar t Empty = t
combinar t1@(Node s1 l1 v1 r1) t2@(Node s2 l2 v2 r2)
    | altura t1 >= altura t2 = rebalance (Node s1 l1 v1 (combinar r1 t2))
    | otherwise = rebalance (Node s2 (combinar t1 l2) v2 r2)


-- Equilibra un árbol binario
rebalance :: BTree a -> BTree a
rebalance Empty = Empty
rebalance t@(Node size izq val der)
    | altura izq > altura der + 1 = rotacionDerecha t
    | altura der > altura izq + 1 = rotacionIzquierda t
    | otherwise = t


-- Rotación a la derecha
rotacionDerecha :: BTree a -> BTree a
rotacionDerecha (Node _ (Node _ ll lv lr) v r) =
    Node (getSize (Node (1 + getSize lr + getSize r) lr v r) + getSize ll) ll lv (Node (1 + getSize lr + getSize r) lr v r)
rotacionDerecha t = t


-- Rotación a la izquierda
rotacionIzquierda :: BTree a -> BTree a
rotacionIzquierda (Node _ l v (Node _ rl rv rr)) =
    Node (getSize (Node (1 + getSize l + getSize rl) l v rl) + getSize rr) (Node (1 + getSize l + getSize rl) l v rl) rv rr
rotacionIzquierda t = t


-- Función que dado un árbol devuelve una lista.
toList :: BTree a -> [a]
toList Empty = []
toList (Node size izq val der) = 
    let listaIzq = toList izq
        listaDer = toList der
    in listaIzq ++ [val] ++ listaDer


-----------------------------------------------------------------------
-- Ejemplo:
-- t1 = Node 5 (Node 3 (Node 1 Empty 1 Empty) 3 (Node 1 Empty 4 Empty)) 5 (Node 1 Empty 7 Empty)
-- splitAt1 t1 3 = (t2, t3)
-- t2 = Node 3 (Node 1 Empty 1 Empty) 3 (Node 1 Empty 4 Empty)
-- t3 = Node 2 Empty 5 (Node 1 Empty 7 Empty)
-- toList t2 = [1, 3, 4]
-- toList t3 = [5, 7]
-- toList t1 = toList t2 ++ toList t3 = [1, 3, 4, 5, 7]