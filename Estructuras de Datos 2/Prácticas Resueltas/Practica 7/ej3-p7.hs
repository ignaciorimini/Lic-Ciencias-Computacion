import Seq
import ListSeq

-- Función hecha por mi: casi.
-- Dada una secuencia de enteros (que representa un histograma), la función devuelve la cantidad de agua almacenada.
aguaHist :: Seq s => s Int -> Int
aguaHist seq =                      -- <2,3,4,7,5,2,3,2,6,4,3,5,2,1>
    let n = lengthS seq             -- 14
        seqIndices = tabulateS id n -- <0,1,2,3,4,5,6...13>
        paresValorIndice = zipS seq seqIndices -- <(2,0),(3,1),(4,2),...>
        triplasValorIzqDer = mapS funAux paresValorIndice -- <(2,<>,seq),(3,<2>,<4,7,5,...>)
        triplasMax = mapS funAux2 triplasValorIzqDer -- <(2,0,7), (3,2,7), ...>
        parValorMinMax = mapS (\(valor, maxIzq, maxDer) -> (valor, min maxIzq maxDer)) triplasMax -- <(2,0), (3,2), ...>
        seqAlturas = mapS (\(valor, minMax) -> minMax - valor ) parValorMinMax
    in reduceS max 0 seqAlturas
    where
        funAux (valor, indice) = (valor, takeS indice seq, dropS (indice + 1) seq)

        funAux2 (valor, seqIzq, seqDer) = 
            let maxIzq = reduceS max 0 seqIzq
                maxDer = reduceS max 0 seqDer
            in (valor, maxIzq, maxDer)


-- Función ayuda IA: mismo concepto pero más eficiente.
aguaHist' :: Seq s => s Int -> Int
aguaHist' alturas =                                                     -- <2,3,4,7,5,2,3,2,6,4,3,5,2,1>
    let (maxIzq, _) = scanS max 0 alturas                               -- (<0,2,3,4,7,7,7,7,7,7,7,7,7,7>, 7)
        (maxDer, _) = scanS max 0 (reverseS alturas)                    -- (<0,1,2,5,5,5,6,6,6,6,6,7,7,7>, 7)
        maxDerReverso = reverseS maxDer                                 -- <7,7,7,6,6,6,6,6,5,5,5,2,1,0>
        seqAlturasParMax = zipS alturas (zipS maxIzq maxDerReverso)     -- <(2, (0,7)), (3, (2,7)), (4, (3,7)), (7, (4,6)), (5, (7,6)), (2, (7,6)), ...>
        agua = mapS funAux seqAlturasParMax                             -- <0, 0, 0, 0, 1, 4, ...>
    in reduceS (+) 0 agua
    where
        funAux (alt, (izq, der)) =
            let borde = min izq der
                agua = borde - alt
            in if agua > 0 then agua else 0


--------------------------------------------
-- Ejemplos.
seq1 = fromList [2,3,4,7,5,2,3,2,6,4,3,5,2,1] :: ListSeq Int