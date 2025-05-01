-- Dada una secuencia de enteros s, se desea determinar el tamaño del siguiente conjunto:
-- multiplos(s) = {(i, j) | 0 <= i < j < |s|, mod(si, sj) == 0}

-- Por ejemplo, multiplos <12, 4, 6, 3, 2> tiene tamaño 7, pues 12 es divisible por 4, 6, 3, 2, luego 4 es divisible por 2, y finalmente 6 es divisible por 3 y por 2. Sumando la cantidad, tenemos 7.

-- Función que dada una secuencia s calcula el tamaño de multiplos(s). Definir la función en término de operaciones paralelizables, como por ejemplo, map y reduce.

-- Función auxiliar que cuenta los múltiplos de un elemento en una secuencia
countMultiples :: (Seq s) => Int -> s Int -> Int
countMultiples x seq = reduceS (+) 0 (mapS (\y -> if y `mod` x == 0 then 1 else 0) seq)

-- Función principal que calcula el tamaño del conjunto multiplos(s)
cantMultiplos :: (Seq s) => s Int -> Int
cantMultiplos s = reduceS (+) 0 counts
  where
    -- Función de acumulación para scanS
    countMultAccum (acc, remainingElems) x =
        let count = countMultiples x remainingElems
        in (acc + count, dropS 1 remainingElems)

    -- scanS aplica countMultiples acumulativamente
    (counts, _) = scanS countMultAccum (0, s) s
