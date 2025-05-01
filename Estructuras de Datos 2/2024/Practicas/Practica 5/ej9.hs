-- Dadas las siguientes definiciones:
data AGTree a = Node a [AGTree a] deriving Show

ponerProfs :: Int -> AGTree a -> AGTree Int
ponerProfs n (Node _ xs) = Node n (map (ponerProfs (n+1)) xs)

{-
Este tipo de datos es un árbol generalizado, donde cada nodo puede tener un número arbitrario de hijos.

La función ponerProfs recorre el árbol y reemplaza los valores de los nodos con su profundidad en el árbol, comenzando desde n para el nodo raíz e incrementando en 1 para cada nivel descendente.

Ejemplo AGTree:
tree = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 [Node 6 []]]]

Luego de ponerProfs 0 tree:
tree = Node 0 [Node 1 [], Node 1 [Node 2 [], Node 2 []]]

______________________________________________
a) Definir una función alturaAGT que calcule la altura de un AGTree.
-}

alturaAGT :: AGTree a -> Int
alturaAGT (Node _ []) = 1
alturaAGT (Node _ xs) = 1 + maxList (map alturaAGT xs)

maxList :: [Int] -> Int
maxList [] = -1
maxList (x:xs) = max x (maxList xs)

{-______________________________________________
b) Definir una función maxAGT que dado un AGTree de enteros devuelva su mayor elemento.
-}

maxAGT :: AGTree Int -> Int
maxAGT (Node n []) = n
maxAGT (Node n xs) = max n (maxList (map maxAGT xs))

-- maxAGT Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]
-- max 1 (maxList (map maxAGT [Node 2 [], Node 3 [Node 4 [], Node 5 []]]))
-- max 1 (maxList [maxAGT (Node 2 []), maxAGT [Node 3 [Node 4 [], Node 5 []]])
-- max 1 (maxList [2, 5])
-- max 1 5
-- 5

-- Es decir, esta función convierte la lista de AGTree en una lista de enteros, en particular, con los mayores enteros de los AGTree: [AGTree] -> [Int].

{-______________________________________________
c) Demostrar que alturaAGT = maxAGT . ponerProfs 1

P(t): alturaAGT t = maxAGT (ponerProfs 1 t)

Demostraremos esta propiedad mediante inducción estructural sobre AGTree.
Dada una propiedad P sobre elementos de tipo AGTree, para probar que vale P(t) para todo t :: AGTree:
- probamos que vale P(Node n [])
- probamos que si vale P(Node n [t1, t2, ... tn]), entonces vale P(Node n [t1, t2, ... tn, tn+1])

------
Caso t = Node n []:
    = alturaAGT (Node n [])                      <alturaAGT 1era def>
    = 1 

    Luego
    = maxAGT (ponerProfs 1 (Node n []))         <ponerProfs def>
    = maxAGT (Node 1 (map ponerProfs 2 []))     <map [] def>
    = maxAGT (Node 1 [])                        <maxAGT 1era def>
    = 1

Luego, se cumple que alturaAGT = maxAGT . ponerProfs 1 en el caso base.

------
Caso inductivo: supongamos que vale P(Node n [t1, t2, ... tn]) (HI) y probemos la propiedad para P(Node n [t1, t2, ... tn, tn+1]).
    = alturaAGT (Node n [t1, t2, ... tn, tn+1])
    = 1 + maxList (map alturaAGT xs)

    Luego
    = ponerProfs 1 (Node n [t1, t2, ... tn])
    = Node 1 (map (ponerProfs (n+1) [t1, t2, ... tn]))
    = 
-}

