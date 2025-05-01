-- aguaHist <2, 3, 4, 7, 5, 2, 3, 2, 6, 4, 3, 5, 2, 1> = 15
-- Dada una secuencia de enteros (que representa un histograma), devuelve la cantidad de agua almacenada, utilizando las funciones scan y reduce.

aguaHist :: Seq Int -> Int
aguaHist s = 
    let (maxLeft, _) = scanS max 0 s
        (maxRightReversed, _) = scanS max 0 (reverseS s)
        maxRight = reverseS maxRightReversed
        seqAgua = zipWith3 (\ml mr h -> max 0 ((min ml mr) - h)) maxLeft maxRight s
    in reduce (+) 0 seqAgua

-- zipWith3 va índice por índice en 3 secuencias distintas realizando la función argumento con cada índice i de cada secuencia.
-- Es decir, toma el índice i de maxLeft, i de maxRight, i de s y realiza la función.