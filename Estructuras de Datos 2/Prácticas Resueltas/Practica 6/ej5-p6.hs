data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- Dado un árbol t y un entero i, construye dos árboles t1 y t2 que contienen: 
-- t1 a los i elementos de más a la izquierda de t, y t2 los restantes.

-- Debe serguir la siguiente especificación.
-- Sean t1 y t2 tales que splitAt t i = (t1,t2), para un árbol t y entero i que satisfacen i <= size t:
-- max (altura t1, altura t2) <= altura t
-- size t1 = i
-- toList t1 ++ toList t2 = toList t
-- WsplitAt(d), SsplitAt(d) son O(d), donde d es la altura del árbol que recibe la función.

splitAt' :: BTree a -> Int -> (BTree a, BTree a)
splitAt' Empty _ = (Empty, Empty)
splitAt' t@(Node s l a r) i
    | i <= 0 = (Empty, t)
    | i >= s = (t, Empty)
    | i < leftSize =
        let (menores, mayores) = splitAt' l i
        in (menores, Node (s - i) mayores a r)
    | i > leftSize =
        let (menores, mayores) = splitAt' r (i - leftSize - 1)
        in (Node i l a menores , mayores)
    | otherwise = (l, Node (s - leftSize) Empty a r)
    where
        leftSize = size l

-- Devuelve el tamaño de una secuencia.
size :: BTree a -> Int
size Empty = 0
size (Node s _ _ _) = s

-- Devuelve los elementos del árbol en una lista inorder.
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node _ l a r) = inorder l ++ [a] ++ inorder r


---------------------------------------------
-- Dado un árbol t, construye un árbol balanceado con los mismos elementos de t (un árbol es balanceado si para cada par de hijos de un nodo cualquiera l1 y l2, la profundidad de los mismos difiere en a lo sumo 1).
rebalance' :: BTree a -> BTree a
rebalance' t = buildBalancedTreeFromList (inorder t)

buildBalancedTreeFromList :: [a] -> BTree a
buildBalancedTreeFromList [] = Empty
buildBalancedTreeFromList xs =
    let n = div (length xs) 2
        (left, (x:right)) = splitAt n xs
        (leftNew, derNew) = (buildBalancedTreeFromList left, buildBalancedTreeFromList right)
    in Node (length xs) leftNew x derNew

-- rebalance :: BTree a -> BTree a
-- rebalance (Node ... (Node ... (Node ... (Node ... ... ... ...) ... ...) ... ...) ... ...) =
-- rebalance (Node ... (Node ... ... ... (Node ... ... ... (Node ... ... ... ...))) ... ...) =
-- rebalance (Node ... ... ... (Node ... ... ... (Node ... ... ... (Node ... ... ... ...)))) =
-- rebalance (Node ... ... ... (Node ... (Node ... (Node ... ... ... ...) ... ...) ... ...)) =
-- rebalance t = t

---------------------------------------------
-- Ejemplos.
t1 :: BTree Int
t1 = Node 6 (Node 3 (Node 1 Empty 7 Empty) 6 (Node 1 Empty 14 Empty)) 8 (Node 2 (Node 1 Empty 10 Empty) 9 Empty)
-- (t2, t3) = splitAt' t1 4
-- inorder t2 ++ inorder t3 -> [7,6,14,8,10,9] = inorder t1