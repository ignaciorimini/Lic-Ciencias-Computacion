{-
1) Un multidiccionario es un diccionario en el que se pueden asociar múltiples valores (no solo uno) a una clave, es decir, es una colección de pares de la forma (clave, valores), donde cada clave puede estar asociada a un único conjunto de valores.

Definimos el TAD MultiDic con las siguientes operaciones:
tad MultiDic (K: Ordered Set, V : Set) where
    import Bool, Conjunto
    empty : Multidic K V
    insert : K -> V -> MultiDic K V -> MultiDic K V     -- agrega un valor al conjunto de valores de una clave
    erase : K -> V -> MultiDic K V -> MultiDic K V      -- elimina un par (clave, valor)
    lookup : K -> MultiDic K V -> Conjunto V            -- devuelve el conjunto de valores asociado a una clave
    isValue : K -> V -> MultiDic K V -> Bool            -- determina si un valor está asociado a una clave
    toMap : MultiDic K V -> Conjunto (K,V)              -- construye el conjunto de (clave, valor) que forman un multidiccionario

___________________________________________________
a) Dar una especificación algebraica para el TAD MultiDic.

lookup k empty = vacio (TAD Conjuntos)

lookup k (insert c v dic) = if k == c 
                            then insertar v (lookup k dic) (insertar es TAD Conjuntos)
                            else lookup k dic 

erase k w (insert c v dic) = if k == c && w == v
                                then erase k w dic
                                else insert c v (erase k w dic)

isValue k w empty = False
isValue k w (insert c v dic) = if k == c && w == v
                                then True
                                else isValue k w dic

toMap empty = vacio (TAD Conjuntos)
toMap (insert c v dic) = insertar (c, v) (toMap dic)


___________________________________________________
1)b) Para implementar las operaciones del TAD MultiDic de manera eficiente en Haskell se utilizan árboles binarios de búsqueda, representados con el siguiente tipo de datos:

data MultiDic k v = E | N (MultiDic k v) (k, Tree v) (MultiDic k v)

donde los valores asociados a las claves se almacenan en árboles binarios que contienen información sobre la cantidad de nodos:

data Tree a = Empty | Leaf a | Node Int (Tree a) (Tree a)

Usando esta representación definir en HAskell de manera eficiente las funciones:

i. isValue :: (Ord k, Eq v) => k -> v -> MultiDic k v -> Bool, del TAD
ii. toMap :: Ord k => MultiDic k v -> Tree (k, Int v), similar a la función toMap del TAD, pero que agrega a cada par de la forma (clave, valor) la posición del valor en la secuencia representada por el árbol. Por ejemplo:

toMap <(1, <a,f,g>), (2, <m,a>)> = <(1,0,a), (1,1,f), (1,2,g), (2,0,m), (2,1,a)>
-}

data MultiDic k v = E | N (MultiDic k v) (k, Tree v) (MultiDic k v)

data Tree a = Empty | Leaf a | Node Int (Tree a) (Tree a)

-- Determina si un valor está asociado a una clave en un multidiccionario.
isValue :: (Ord k, Eq v) => k -> v -> MultiDic k v -> Bool
isValue k v E = False
isValue k v (N l (c, valores) r)
    | k == c = isInTree v valores
    | k < c = isValue k v l
    | otherwise = isValue k v r
    where
        isInTree :: v -> Tree v -> Bool
        isInTree v Empty = False
        isInTree v (Leaf w) = v == w
        isInTree v (Node size l r) = isInTree v l || isInTree v r

-- Construye el conjunto de (clave, valor) que forman un multidiccionario
toMap :: Ord k => MultiDic k v -> Tree (k, Int, v)
toMap m = toMap' m Empty

toMap' :: Ord k => MultiDic k v -> Tree (k, Int v) -> Tree (k, Int, v)
toMap' E t = t
toMap' (N l (c, treeValores) r) t =
    let arbolClaveValor = mapTree (\valor -> (c, valor)) treeValores
        arbolClaveValorPlano = ...
        leftToMap = toMap' ...
        rightToMap = toMap' ...
    in Node 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node size l r) = 
    let (izqMap, derMap) = (mapTree f l, mapTree f r)
    in Node size izqMap derMap


{-___________________________________________________
1)c) Usando esta representación, se define la función erase como:

erase :: (Ord k, Eq v) => k -> v -> MultiDic k v -> MultiDic k v
erase k v (N l (k', vs) r)
    | k == k' = N l (k, elim v vs) r
    | k < k' = N (erase k v l) (k', vs) r
    | k > k' = N l (k', vs) (erase k v r)
erase k v E = E

donde la función elim elimina un elemento de un árbol.

Probar por inducción estructural sobre t la siguiente propiedad:
"Si t es un bst, erase k v t es un bst, para cualesquiera k y v"
-}

{-___________________________________________________
Definición. Inducción estructural sobre MultiDic.
- P(E) vale.
- Si vale P(l) y P(r) entonces vale P(N l (k, vs) r).

Propiedad a probar. P(t): si t es un bst, erase k v t es un bst, para cualesquiera k y v.

- Caso t = E: 
= erase k v t
= erase k v E
= E             <erase 2da def>
Como E es un bst por definición, entonces se cumple el caso base.

- Caso t = (N l (k',vs) r).
Suponemos las siguientes hipótesis inductivas:
H1) Vale P(l): l es un bst y erase k v l es un bst.
H2) Vale P(r): r es un bst y erase k v r es un bst.
Y probaremos la propiedad para (N l (k',vs) r).

erase k v (N l (k',vs) r), tenemos 3 casos:

- Si k == k' -> N l (k, elim v vs) r
Como por hipótesis inductiva l y r son BST, y k' no se modifica, entonces N l (k, elim v vs) r es un bst.

- Si k < k' -> N (erase k v l) (k', vs) r
Por hipótesis inductiva, l es un bst y por lo tanto "erase k v l" también es un bst. Como erase no modifica las claves entonces erase k v t es un bst.

- Si k > k' -> N l (k', vs) (erase k v r)
Por hipótesis inductiva, r es un bst y por lo tanto "erase k v r" también es un bst. Como erase no modifica las claves, entonces erase k v t es un bst.

Finalmente, P(t) se cumple para todo t :: MultiDic.
-}