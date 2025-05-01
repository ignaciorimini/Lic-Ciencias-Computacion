{-
El problema de calcular la maxima suma de una subsecuencia contigua de una secuencia dada s puede resolverse con un algoritmo “Divide & Conquer” que en cada llamada recursiva calcule: la maxima suma de una subsecuencia contigua de s, la maxima suma de un prefijo de s, la maxima suma de un sufijo de s y la suma de todos los elementos de s. Dado el siguiente tipo de datos:
-}

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

-- Ejemplo: secuencia <2, -3, 4, 1>
-- s = Join (Join (Leaf 2) (Leaf (-3))) (Join (Leaf 4) (Leaf 1))

-- Ejemplo: secuencia <2, -3, 4, 1, 5>
-- s = Join (Join (Leaf 2) (Leaf (-3))) (Join (Join (Leaf 4) (Leaf 1)) (Leaf 5))


-- Función que calcula la máxima suma de una subsecuencia contigua de una secuencia dada, en términos de mapreduce.
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
-- acc = acumulador


-- TRABAJO DE MCSS
-- W(n) = Wmap(n) + Wcombine(n) = 