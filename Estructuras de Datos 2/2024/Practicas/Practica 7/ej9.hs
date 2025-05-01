-- a) Función countCaract.
-- Dada una colección de textos, calcula la cantidad de veces que aparecen los caracteres en los textos. Definirla en términos de mapCollectReduce.

-- <<"h", "e", "l", "l", "o">, <"w", "o", "r", "l", "d">>
-- <<("h", 1), ("e", 1), ...>, <("w", 1), ("0", 1), ...>>
-- <("h", 1), ("e", 1), ... ("l", 1), ("l", 1), ...>
-- <("h", <1>), ("e", <1>), ("l", <1, 1, 1>), ...>
-- <("h", 1), ("e", 1), ("l", 3)

countCaract :: Seq (Seq Char) -> Seq (Char, Int)
countCaract s
    | isEmptyS s = emptyS
    | otherwise = 
        let s1 = map (\seq -> map (\c -> (c, 1)) seq) s
            s2 = reduce appendS emptyS s1 -- join s1
            s3 = collectS s2
            s4 = map (\(c, seq) -> (c, reduceS (+) 0 seq)) s3
        in s4


