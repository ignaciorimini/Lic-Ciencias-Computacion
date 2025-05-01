{-
Definir las siguientes funciones, correspondientes a la interfaz de Secuencias, implementando las secuencias con árboles binarios, definidos con el siguiente tipo de datos, donde se almacenan los tamaños de los árboles en los nodos. 

Suponer que el recorrido inorder del arbol da el orden de los elementos de la secuencia. Calcular el trabajo y la profundidad de cada una. Resolver la recurrencia y expresar la solucion en terminos del orden O.
-}

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-------------------------------------
-- FUNCIÓN NTH
-- Calcula el n-ésimo elemento de una secuencia.
nth :: BTree a -> Int -> a
nth Empty _ = error "Árbol vacío."                          -- O(1)
nth (Node size izq val der) n
    | n < 0 || n >= size = error "Índice fuera de rango."   -- O(1)
    | n == leftSize = val                                   -- O(1)
    | n < leftSize = nth izq n                              -- O(lg n)
    | otherwise = nth der (n - leftSize - 1)                -- O(lg n)
    where
        leftSize = sizeOfTree izq


-- Función auxiliar para obtener el tamaño de un árbol
sizeOfTree :: BTree a -> Int
sizeOfTree Empty = 0
sizeOfTree (Node size _ _ _) = size


-- Ejemplo:
-- s = <1, 2, 3, 4, 5, 6>
-- s = Node 6 (Node 3 (Node 1 Empty 1 Empty) 2 (Node 1 Empty 3 Empty)) 4 (Node 2 (Node 1 Empty 5 Empty) 6 Empty)
-- nth s 1 -> 2
-- nth s 2 -> 3

-- TRABAJO W(n) DE NTH
-- Si n < leftSize, la recurrencia es: W(n) = W(Tizq) + O(1)
-- Si n > leftSize, la recurrencia es: W(n) = W(Tder) + O(1)
-- Para un árbol balanceado, donde el tamaño del subárbol izquierdo y derecho es aproximadamente la mitad del tamaño del árbol original, tenemos:
-- W(n) = W(n/2) + O(1)
-- Según el teorema maestro: a=1, b=2 y c=0 -> O(lg n)

-- Pero si el árbol no está balanceado, y tiene una sola rama profunda como si fuese una lista enlazada, W(n) puede cambiar:
-- W(n) = W(n - 1) + O(1) -> W(n) = O(n)

-- PROFUNDIDAD DE NTH
-- S(n) = S(n/2) + O(1)
-- Por teorema maestro -> O(lg n)


-------------------------------------
-- FUNCIÓN CONS
-- Inserta un elemento al comienzo de la secuencia.

cons :: a -> BTree a -> BTree a
cons arg Empty = (Node 1 Empty arg Empty)
cons arg (Node size izq val der) = (Node newSize (cons arg izq) val der)
    where
        newSize = size + 1

-- TRABAJO DE CONS
-- Si el árbol está balanceado:
-- W(n) = W(n/2) + O(1)
-- Por teorema maestro: a=1, b=2, c=0 -> O(lg n)

-- Si el árbol es una lista enlazada:
-- W(n) = W(n - 1) + O(1)
-- W(n) = O(n)

-- PROFUNDIDAD DE CONS
-- Si el árbol está balanceado.
-- S(n) = S(n/2) + O(1)
-- Por teorema maesotr: a=1, b=2, c=0 -> O(lg n)


-------------------------------------
-- FUNCIÓN TABULATE
-- Dada una función f y un entero n, devuelve una secuencia de tamaño n, donde cada elemento de la secuencia es el resultado de aplicar f al índice del elemento.

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f n = 
    if n <= 0
        then Empty
        else (Node n (tabulate f (n-1)) (f (n-1)) Empty)

-- TRABAJO DE TABULATE
-- W(n) = W(n - 1) + O(1)
-- W(n) = O(n)

-- PROFUNDIDAD DE TABULATE
-- S(n) = S(n - 1) + O(1)
-- S(n) = O(n)


-------------------------------------
-- FUNCIÓN MAP
-- Dada una función f y una secuencia s, devuelve el resultado de aplicar f sobre cada elemento de s.

map' :: (a -> b) -> BTree a -> BTree b
map' f Empty = Empty
map' f (Node size izq val der) = (Node size (map' f izq) (f val) (map' f der))

-- Funciones y definiciones auxiliares para probar map.
testTree = Node 2 (Node 1 Empty 5 Empty) 10 Empty

double :: Int -> Int
double x = x * 2

mapResult :: BTree Int
mapResult = map' double testTree

-- Devuelve Node 2 (Node 1 Empty 10 Empty) 20 Empty


-------------------------------------
-- FUNCIÓN TAKE
-- Dados un entero n y una secuencia s, devuelve los primeros n elementos de s.

take' :: Int -> BTree a -> BTree a
take' _ Empty = Empty
take' n (Node size izq val der)
    | n <= 0 = Empty
    | n >= size = (Node size izq val der)
    | n == leftSize = izq
    | n == (leftSize + 1) = (Node n izq val Empty)
    | n < leftSize = take' n izq
    | otherwise = (Node (leftSize + 1) izq val (take' (n - leftSize - 1) der))
    where
        leftSize = sizeOfTree izq


-------------------------------------
-- FUNCIÓN DROP
-- Dados un entero n y una secuencia s, devuelve la secuencia s sin los primeros n elementos.

drop' :: Int -> BTree a -> BTree a
drop' _ Empty = Empty
drop' n (Node size izq val der)
    | n <= 0 = (Node size izq val der)
    | n >= size = Empty
    | n == leftSize = (Node (size - n) Empty val der)
    | n == (leftSize + 1) = 
        if isEmpty der
            then (Node (size - n) Empty val Empty)
            else der
    | n < leftSize = (Node (size - n) (drop' n izq) val der)
    | otherwise = drop' (n - leftSize) der
    where
        leftSize = sizeOfTree izq

isEmpty :: BTree a -> Bool
isEmpty Empty = True
isEmpty _ = False