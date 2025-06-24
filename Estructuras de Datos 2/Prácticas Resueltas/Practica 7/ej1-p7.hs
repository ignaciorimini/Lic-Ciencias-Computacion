import Seq
import ListSeq

-- Dada una secuencia de enteros calcula el promedio de cada comienzo de la lista.
-- promedios <2,4,3,7,9> = <2,3,3,4,5>
promedios :: Seq s => s Int -> s Float
promedios seq =
    let (secSumasParcialesSinUltimo, sumaTotal) = scanS (+) 0 seq                       -- (<0, 0+2, 0+2+4, 0+2+4+3, 0+2+4+3+7>, 0+2+4+3+7+9)
        secIndices = tabulateS fromIntegral (lengthS secSumasParcialesSinUltimo + 1)    -- <0.0, 1.0, 2.0, 3.0, 4.0, 5.0>
        secSumasParciales = appendS secSumasParcialesSinUltimo (singletonS sumaTotal)   -- <0, 0+2, 0+2+4, 0+2+4+3, 0+2+4+3+7, 0+2+4+3+7+9>
        secSumasIndices = zipS secSumasParciales secIndices                             -- <(0,0), (0+2, 1), (0+2+4,2), ...>
        secSumasIndicesSinCero= dropS 1 secSumasIndices                                 -- Para eliminar el primer elemento (0,0).
    in mapS (\(suma, divisor) -> fromIntegral suma / divisor) secSumasIndicesSinCero


-- Dada una secuencia de enteros devuelve la cantidad de enteros en la secuencia que son mayores a todos los anteriores.
-- mayores <1,2,5,3,5,2,7,9> = 4
mayores :: Seq s => s Int -> Int
mayores seq =                                                                               -- <1,2,5,3,5,2,7,9>
    let (prefixMaximos, ult) = scanS max minBound seq                                       -- (<minBound, 1, 2, 5, 5, 5, 5, 7>, 9)
        seqMaximosPrev = dropS 1 prefixMaximos                                              -- <1,2,5,5,5,5,7>
        seqValorMax = zipS (dropS 1 seq) seqMaximosPrev                                     -- <(2,1), (5,2), (3,5), (5,5), (2,5), (7,5), (9,7)>
        mayoresQueAnteriores = filterS (\(actual, prevMax) -> actual > prevMax) seqValorMax -- <(2,1), (5,2), (7,5), (9,7)>
    in lengthS mayoresQueAnteriores

--------------------------------------------
-- Ejemplos.
seq1 = fromList [2,4,3,7,9] :: ListSeq Int
seq2 = fromList [1,2,5,3,5,2,7,9] :: ListSeq Int
seq3 = fromList [1,2,3,4,5,6,7,8] :: ListSeq Int
