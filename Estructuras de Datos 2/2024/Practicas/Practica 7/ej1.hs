-- a) Función promedios.
-- Dada una secuencia de enteros, calcula el promedio de cada comienzo de la lista.
promedios :: Seq Int -> Seq Float
promedios s = 
    let (sums, _) = scan (+) 0 s
        n = length s
        indices = tabulate (+ 1) n
        avg (sum, idx) = fromIntegral sum / fromIntegral idx
    in map avg (zipWith (,) sums indices)

-- zipWith: (a -> b -> c) -> Seq a -> Seq b -> Seq c
-- Combina dos secuencias en una secuencia de tuplas aplicando una función a pares de elementos correspondientes.

promedios :: Seq s => s Int -> s Float
promedios seq =
    let (sums, _) = scanS (+) 0 seq            -- Paso 2: Calcular la suma acumulada
        indexedSeq = enumerateS seq             -- Paso 3: Obtener una secuencia de tuplas (elemento, índice)
        averages = mapS avg indexedSeq         -- Paso 4: Calcular el promedio para cada inicio de lista
    in averages

avg :: (Int, Int) -> Float
avg (sum, idx) = fromIntegral sum / fromIntegral idx


-----------------------------------------
-- b) Función mayores.
-- Dada una secuencia de enteros, devuelve la cantidad de enteros en la secuencia que son mayores a todos los anteriores. Por ejemplo,
-- mayores <1, 2, 5, 3, 5, 2, 7, 9> = 4 (que son: 2, 4, 7, 9).

mayores :: Seq Int -> Int
mayores s =
    let (restas, _) = scan (-) 0 s
        boolSeq = map (> 0) restas
    in countTrues boolSeq
    where
        countTrues s' = foldrS (\x acc -> if x then acc + 1 else acc) 0 s'