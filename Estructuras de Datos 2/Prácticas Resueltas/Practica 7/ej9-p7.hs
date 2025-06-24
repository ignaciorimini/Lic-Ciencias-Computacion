-- Dada una colección de textos calcula la cantidad de veces con que aparecen los caracteres en los textos. Definirla en términos de mapCollectReduce.
countCaract :: Seq s => s (s Char) -> s (Char, Int)
countCaract seq =
    let seqPlana = joinS seq                                                                                        -- W(n) = O(n) | S(n) = O(lg n)
        seqParLetraValor = mapS (\char -> (char, 1)) seqPlana                                                       -- W(n) = O(n) | S(n) = O(1)
        seqCollected = collectS seqParLetraValor                                                                    -- W(n) = O(n lg(n)) | S(n) = O(lg^2(n))
        seqLetraAparicion = mapS (\(letra, seqApariciones) -> (letra, reduceS (+) 0 seqApariciones)) seqCollected   -- W(n) = O(n) | S(n) = O(lg n)
    in seqLetraAparicion

-- En conclusión, la función es eficiente y paralelizable. Total:
-- W(n) = O(n) + O(n) + O(n lg(n)) + O(n) = O(n lg(n))
-- S(n) = O(lg n) + O(1) + O(lg^2(n)) + O(lg n) = O(lg^2(n))

-- Ejemplo.
-- seq = <<'a','b','c'>, <'a','d','c'>, <'e','f','c'>>
-- seqPlana = <'a','b','c','a','d','c','e','f','c'>
-- seqParLetraValor = <('a',1), ('b',1), ('c',1), ('a',1), ('d',1), ...>
-- seqCollected = <('a', <1,1>),  ('b', <1>), ('c', <1,1,1>), ...>
-- seqLetraAparicion = <('a',2), ('b',1), ('c',3), ...>


----------------------------------------------
-- Dada una colección de textos calcula las frecuencias con que cada caracter aparece en los textos. La secuencia resultado debe contener pares de la forma (n, caracteres con frecuencia n) y debe estar ordenada según las frecuencias de los caracteres. Utilizar las funciones countCaract, map y collect.
huffman :: Seq s => s (s Char) -> s (Int, s Char)
huffman seq =
    let seqLetraAparicion = countCaract seq
        seqAparicionLetra = mapS (\(letra, cant) -> (cant, letra)) seqLetraAparicion
        seqCollected = collectS seqAparicionLetra
    in seqCollected

-- Ejemplo.
-- seq = <<'a','b','c'>, <'a','d','c'>, <'e','f','c'>>
-- seqLetraAparicion = <('a',2), ('b',1), ('c',3), ...>
-- seqAparicionLetra = <(2,'a'), (1,'b'), (3,'c'), ...>
-- seqCollected = <(2,<a,e,f>), (1,<b>), (3,<c>), ...>