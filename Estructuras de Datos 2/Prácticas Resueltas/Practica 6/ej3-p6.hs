{-
Dados los diferentes valores de las acciones de YPF a lo largo del tiempo, se desea saber cuál es la mejor ganancia que se puede obtener al comprar acciones un día y venderlas otro.

Definir una función mejorGanancia :: Tree Int -> Int que calcule la mejor ganancia dada una secuencia de valores, utilizando el siguiente algoritmo:
- Armar pares de la forma (compra, ventas), donde compra es el precio al cual se puede comprar una acción y ventas los distintos valores en que puede venderse.
- Para cada par de la forma (compra, ventas) calcular las diferencias venta - compra, donde venta es un elemento de ventas.
- Tomar el número máximo de las diferencias calculadas en el paso anterior.
-}

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

-- Dado un árbol t, construye otro con los sufijos de cada elemento de t.
sufijos :: Tree Int -> Tree (Tree Int)
sufijos t = arbolSufijos
    where
        -- Indexa cada árbol segun su índice. Usamos esta función en vez de zipT con un árbol hecho con tabulate para mantener la estructura.
        indexTree :: Tree a -> Int -> (Tree (a, Int), Int)
        indexTree E i = (E, i)
        indexTree (Leaf x) i = (Leaf (x, i), i + 1)
        indexTree (Join l r) i =
            let (lIndexed, i2) = indexTree l i
                (rIndexed, i3) = indexTree r i2
            in (Join lIndexed rIndexed, i3)

        -- Función para aplicar en cada par del árbol (valor, índice + 1). Se utiliza para dropear el propio elemento.
        dropIndice :: (Int, Int) -> Tree Int
        dropIndice (_, i) = dropT (i+1) t

        -- Árbol de tipo (valor, índice)
        arbolValorIndice = fst (indexTree t 0)

        -- Árbol de tipo Tree (Tree Int) donde en cada valor del árbol anterior, hay un árbol que representa los sufijos.
        arbolSufijos = mapTree dropIndice arbolValorIndice


-- Dado un árbol t reemplaza cada elemento v de t por el par (v, sufijos de v en t).
conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = zipT t (sufijos t)


-- Calcula el máximo elemento de un árbol de enteros. Definir maxT en términos de reduce.
maxT :: Tree Int -> Int
maxT t = reduceTree max 0 t


-- Calcula el máximo elemento de un árbol de árboles de enteros. Definir maxAll en términos de mapreduce.
maxAll :: Tree (Tree Int) -> Int
maxAll t = reduceTree max 0 (mapTree maxT t)


-- Calcula la mejor ganancia dada una secuencia de valores.
mejorGanancia :: Tree Int -> Int
mejorGanancia t = maxGanancia
    where
        -- Obtiene un árbol (valor, árbol de sufijos)
        arbolValorSufijos = conSufijos t

        -- A cada nodo del árbol anterior le aplicamos la función restaVentaCompra, 
        -- que toma cada par (valor, árbol de sufijos), y reduce el árbol de sufijos en la mejor ganancia posible.
        -- Aplica un map que resta a cada valor del árbol de sufijos, el valor del nodo (venta - compra).
        -- Luego, reduce dicho árbol para obtener la máxima diferencia (venta - compra).
        arbolValorDifMax = mapTree restaVentaCompra arbolValorSufijos

        -- En esta instancia tiene un árbol con las mejores diferencia (venta - compra) en cada instante, por lo que
        -- simplemente toma la mayor ganancia posible del árbol.
        maxGanancia = maxT arbolValorDifMax

        restaVentaCompra :: (Int, Tree Int) -> Int
        restaVentaCompra (valor, t) = reduceTree max 0 (mapTree (\x -> x - valor) t)


----------------------------------------
-- Funciones auxiliares.
indexTree :: Tree a -> Tree Int
indexTree t = fst (go 0 t)
  where
    go :: Int -> Tree a -> (Tree Int, Int)
    go i E = (E, i)
    go i (Leaf _) = (Leaf i, i+1)
    go i (Join l r) =
      let (l', i1) = go i l
          (r', i2) = go i1 r
      in (Join l' r', i2)


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f E = E
mapTree f (Leaf val) = Leaf (f val)
mapTree f (Join izq der) = Join (mapTree f izq) (mapTree f der)

reduceTree :: (a -> a -> a) -> a -> Tree a -> a
reduceTree _ acc E = acc
reduceTree f acc (Leaf x) = f acc x
reduceTree f acc (Join left right) = reduceTree f (reduceTree f acc left) right

size :: Tree a -> Int
size E = 0
size (Leaf _) = 1
size (Join l r) =
    let (izqSize, derSize) = (size l, size r)
    in izqSize + derSize

zipT :: Tree a -> Tree b -> Tree (a,b)
zipT E E = E
zipT (Leaf x) (Leaf y) = Leaf (x,y)
zipT (Join l1 r1) (Join l2 r2) = Join (zipT l1 l2) (zipT r1 r2)
zipT _ _ = E

dropT :: Int -> Tree a -> Tree a
dropT n E = E
dropT n t@(Join l r)
    | n <= 0 = t
    | n >= treeSize = E
    | n < leftSize = Join (dropT n l) r
    | n > leftSize = dropT (n - leftSize - 1) r
    | otherwise = r
    where
        treeSize = size t
        leftSize = size l


----------------------------------------
-- Ejemplos.
t1 :: Tree Int
t2 :: Tree (Tree Int)
t3 :: Tree Int
t1 = Join (Join (Leaf 10) (Leaf 15)) (Join (Leaf 20) (Leaf 45)) -- Mejor ganancia es 45 - 10 = 35
t2 = Join (Join (Leaf (Join (Leaf 15) (Leaf 20))) (Leaf (Leaf 20))) (Leaf E)
t3 = Join (Join (Leaf 1) (Leaf 2)) (Leaf 3)