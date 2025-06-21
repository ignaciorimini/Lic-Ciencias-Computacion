{-
El problema de calcular la máxima suma de una subsecuencia contigua de una secuencia dada s puede resolverse con un algoritmo "Divide and Conquer" que en cada llamada recursiva calcule: la máxima suma de una subsecuencia contigua de s, la máxima suma de un prefijo de s, la máxima suma de un sufijo de s y la suma de todos los elementos de s. Dado el siguiente tipo de datos:

data Tree a = E | Leaf a | Join (Tree a) (Tree a)

a) Definir una función mcss :: (Num a, Ord a) => Tree a -> a, que calcule la máxima suma de una subsecuencia contigua de una secuencia dada, en términos de mapreduce.

Ayuda: dado un árbol t, mcss aplica la función reduce sobre el árbol que se obtiene al reemplazar cada elemento v por la 4-upla (max (v,0), max(v,0), (max v,0), v).
-}

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

-- Función que calcula la máxima suma de una subsecuencia contigua de una secuencia dada, en términos de mapreduce. La 4-upla devuelve:
-- 1. el resultado deseado
-- 2. el máximo prefijo
-- 3. el máximo sufijo
-- 4. la suma total
mcss :: (Num a, Ord a) => Tree a -> a
mcss s = maxSum
    where
        (maxSum, _, _, _) = reduceTree combine val (mapTree base s)
        val = (0, 0, 0, 0)
        base v =
            let v' = max v 0
            in (v', v', v', v)
        combine :: (Num a, Ord a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
        combine (m, p, s, t) (m', p', s', t') = (max (max (s + p') m) m', max p (t + p'), max s' (s + t'), t + t')


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f E = E
mapTree f (Leaf val) = Leaf (f val)
mapTree f (Join izq der) = Join (mapTree f izq) (mapTree f der)


reduceTree :: (a -> a -> a) -> a -> Tree a -> a
reduceTree _ acc E = acc
reduceTree f acc (Leaf x) = f acc x
reduceTree f acc (Join left right) = reduceTree f (reduceTree f acc left) right


-- Ejemplos.
-- Secuencia <2, -3, 4, 1>
s1 = Join (Join (Leaf 2) (Leaf (-3))) (Join (Leaf 4) (Leaf 1))

-- Secuencia <2, -3, 4, 1, 5>
s2 = Join (Join (Leaf 2) (Leaf (-3))) (Join (Join (Leaf 4) (Leaf 1)) (Leaf 5))


{-
Calcular el trabajo y la profundidad de mcss.
...
-}