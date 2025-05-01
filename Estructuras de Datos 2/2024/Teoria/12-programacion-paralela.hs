{-
PROGRAMACIÓN EN PARALELO

__________________________
MERGESORT
El algoritmo de ordenación mergesort es un clásico ejemplo de Divide & Conquer: dividimos la entrada en dos (split), ordenamos recursivamente y juntamos las dos mitades ordenadas (merge).

-----------------------
Definición mergesort.

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs =
    let (ls, rs) = split xs
        (ls', rs') = (msort ls || msort rs)
    in merge (ls', rs')

split :: [Int] -> [Int] x [Int]
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) =
    let (xs, ys) = split zs
    in (x:xs, y:ys)

merge :: [Int] x [Int] -> [Int]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x:xs, y:ys) =
    if x <= y
        then x:merge(xs, y:ys)
        else y:merge(x:xs, ys)

-----------------------
Trabajo mergesort.

Wsplit(n) -> O(n)
Wmerge(n) -> O(n)
Wmsort(n) = c0n + 2Wmsort(n/2) + c1n + c2 -> O(n log(n))

-----------------------
Profundidad mergesort.

Ssplit(n) -> O(n)
Smerge(n) -> O(n)
Smsort(n) = k0n + Smsort(n/2) + k1n + k2 -> O(n)

Este resultado de Smsort nos demuestra que mergesort no es muy paralelizable.

__________________________
PARALELIZANDO MERGESORT
Si recordamos la definición de paralelismo, P = log(n) para este algoritmo, lo cual nos dice que muy pocos procesadores pueden usarse de forma eficiente.

El problema aquí es la profundidad, ya que el trabajo no puede ser mejorado (se puede demostrar que cualquier algoritmo de ordenamiento basado en comparaciones es al menos n log(n) en el trabajo).

Si analizamos la recurrencia de la profundidad msort,
Smsort(n) = Ssplit(n) + Smsort(n/2) + Smerge(n) + k
Notamos que son las funciones merge y split quienes incurren un término lineal en la recurrencia. ¿Se pueden entonces mejorar las profundidades de ambas?

La respuesta es sí y no. Sí se pueden mejorar las profundidades de las tres funciones, pero no trabajando sobre listas. El problema no es el algoritmo de mergesort, sino la utilización de listas. En general las listas no son buenas para paralelizar: su modelo de construcción es claramente secuencial, nodo tras nodo hasta llegar al final.

La elección de la estructura de datos influye en la profundidad del algoritmo. En lugar de listas trabajaremos con el siguiente tipo de árboles:

data BT a = Empty | Node (BT a) a (BT a)

Paralelización más efectiva: Debido a la estructura jerárquica de los árboles binarios de búsqueda, es más fácil dividir y procesar los datos de manera paralela. Cada subárbol puede ser procesado de forma independiente, lo que facilita la paralelización del algoritmo y puede llevar a una mejor utilización de los recursos de hardware en entornos paralelos.

Veremos que podemos implementar msort sobre árboles con W(n) = O(n log(n)) y S(n) = O(log(n^3))

__________________________
ÁRBOLES DE BÚSQUEDA
Un elemento de BT a está ordenado si y sólo si:
    1. Es el árbol Empty.
    2. Es un (Node izq x der) y además,
        - izq está ordenado
        - der está ordenado
        - todos los elementos de izq son <= x
        - todos los elementos de der son > x
    3. Un árbol ordenado induce una lista ordenada (inorder):

listar :: BT a -> [a]
listar Empty = []
listar (Node izq x der) = listar izq ++ [x] ++ listar der

__________________________
MERGESORT SOBRE ÁRBOLES

-----------------------
Split en mergesort sobre árboles.

Tenemos que Wsplit = O(1) y Ssplit = O(1), pues el árbol ya está divido en sus subárboles izquierdo y derecho.

-----------------------
Msort en mergesort sobre árboles.

msort :: BT a -> BT a
msort Empty = Empty
msort (Node izq x der) =
    let (izq', der') = msort izq || msort der
    in merge (merge izq' der') (Node Empty x Empty)

-----------------------
Merge en mergesort sobre árboles.

Queremos definir la función merge tal que Wmerge = O(n) y Smerge sea mejor que O(n). ¿Como lo definimos?

1) Consideremos el caso. 
merge (Node (Empty 1 Empty) 3 (Empty 5 Empty)) (Node (Empty 2 Empty) 4 (Empty 6 Empty))

2) Elegimos guiarnos por el primer argumento.
= Node (merge ...) 3 (merge ...)

3) El 1 va a la izquierda, el 5 a la derecha.
= Node (merge (Node Empty 1 Empty) ...) 3 (merge (Node Empty 5 Empty))

4) Separamos el segundo argumento en árboles menores a 3 y mayores a 3.
= Node (merge (Node Empty 1 Empty) (Node Empty 2 Empty)) 3 (merge (Node Empty 5 Empty) (Node Empty 4 (Node Empty 6 Empty)))

5) Finalmente:
= Node (Node Empty 1 (Node Empty 2 Empty)) 3 (Node (Node Empty 4 Empty) 5 (Node Empty 6 Empty))
= [1, 2, 3, 4, 5, 6] (inorder)

6) Implementación:

merge :: BT Int -> BT Int -> BT Int
merge Empty t2 = t2
merge (Node izq1 x izq2) t2 =
    let (izq2, der2) = splitAt t2 x
        (izq', der') = (merge izq1 izq2) || (merge der1 der2)
    in Node izq' x der'

splitAt :: BT Int -> Int -> BT Int x BT Int
splitAt Empty _ = (Empty, Empty)
splitAt (Node l x r) y =
    if y < x
        then let (ll, lr) = splitAt l y
            in (ll, Node lr x r)
        else let (rl, rr) = splitAt r y
            in (Node l x rl, rr)

__________________________
PROFUNDIDAD DE MERGESORT

------------
Profundidad de SplitAt.
Sea h la altura del árbol:
SsplitAt(h) = k + SsplitAt(h - 1) -> O(h)

------------
Profundidad de Merge.
Sean h1 y h2 las alturas de los árboles argumento:
Smerge(h1, h2) <= k + SsplitAt(h2) +  max Smerge(h1-1, h21) Smerge(h1-1, h22)

Donde h21 y h22 son las alturas de los árboles devueltos por splitAt. Como h21, h22 <= h2, entonces:

Smerge(h1, h2)  <= k + SsplitAt(h2) + max Smerge(h1-1, h2) Smerge(h1-1, h2)
                <= k'h2 + Smerge(h1 - 1, h2)

Si sumamos h1 veces (k'h2), entonces Smerge(h1, h2) = O(h1 . h2)

Si n es el tamaño del árbol, y el árbol está balanceado, entonces h = O(log(n))

------------
Profundidad de Msort.

1.....................
Smsort(n) <= k + max Smsort(n/2) Smsort(n/2) + Smerge(lg n, lg n) + Smerge (2 lg n, 1)

El (2 lg n) es porque altura (merge i r) <= altura i + altura r.

2.....................
Como Smerge(h1, h2) = O(h1 . h2), entonces:
Smsort(n) <= k + Smsort(n/2) + k1 (log n)^2 + k2 log(n)
Smsort(n) <= k + Smsort(n/2) + k3 (log n)^2

Por lo tanto, Smsort(n) = O((log(n))^3)

__________________________
FUNCIÓN REBALANCE
El análisis de la profundidad tiene un error grave. La profundidad de merge suponía árboles balanceados, pero en msort llamamos a merge con el resultado de las llamadas recursivas.

Lo arreglamos con una función que rebalancee el árbol: rebalance.

msort :: BT a -> BT a
msort Empty = Empty
msort (Node izq x der) =
    let (izq', der') = msort izq || msort der
    in rebalance (merge (merge izq' der') (Node Empty x Empty))

Este algoritmo paralelo trabaja sobre árboles, pero la entrada podría ser una lista. Convertir una estructura secuencial en paralela puede no ser paralelizable, por lo tanto no podríamos esperar una mejora lineal en la cantidad de procesadores.

__________________________
FUNCIONES DE ALTO ORDEN EN ÁRBOLES
Trabajaremos ahora sobre otra variante de árboles binarios:
data T a = Empty | Leaf a | Node (T a) (T a)

-------------
FUNCIÓN MAP
Recibe una función y se la aplica a cada elemento de un árbol dado.

mapT :: (a -> b) -> T a -> T b
mapT f Empty = Empty
mapT f (Leaf x) = Leaf (f x)
mapT f (Node l r) =
    let (l', r') = mapT f l || mapT f r
    in Node l' r'

Si suponemos que Wf = O(1) y Sf = O(1) entonces
- Wmap = O(n)
- Smap = O(lg n)

-------------
FUNCIÓN SUM
Suma todos los elementos de un árbol de enteros.

sumT :: T Int -> Int
sumT Empty = 0
sumT (Leaf x) = x
sumT (Node l r) =
    let (l', r') = sumT l || sumT r
    in l' + r'

- Wsum = O(n)
- Ssum = O(lg n)

-------------
FUNCIÓN FLATTEN
Aplana un árbol de cadenas


-}