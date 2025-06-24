import Seq
import ListSeq

----------------------------------------------
-- El problema de "la subsecuencia contigua creciente más larga" consiste en encontrar la cantidad mayor de crecimientos contiguos en una secuencia. Por ejemplo:
-- sccml <9,3,5,1,3,4,5,6,8,1> = 5
-- sccml <5,6,2,3,5,1,9> = 2
-- sccml <1,4,6,7,8,11,12,3> = 6

----------------------------------------------
-- a) Definir sccml :: Seq Int -> Int, que resuelva el problema de la "subsecuencia contigua creciente más larga", utilizando un algoritmo "Divide and Conquer".

sccmlDyc :: Seq s => s Int -> Int
sccmlDyc seq = 
    let (maxLong, _, _, _, _) = sccmlDycAux seq
    in maxLong

-- Tuppling: (a,b,c,d,e)
-- a: Resultado deseado: longitud de la subsecuencia creciente más larga en el segmento.
-- b: primer valor del segmento.
-- c: último valor del segmento.
-- d: longitud del prefijo creciente (desde el inicio del segmento).
-- e: longitud del sufijo creciente (hasta el final del segmento).
sccmlDycAux :: Seq s => s Int -> (Int, Int, Int, Int, Int)
sccmlDycAux seq = case showtS seq of
    EMPTY -> (0,0,0,0,0)
    ELT v -> (0,v,v,0,0) 
    NODE l r -> 
        let (a1, firstL, lastL, preL, sufL) = sccmlDycAux l
            (a2, firstR, lastR, preR, sufR) = sccmlDycAux r
            -- Si hay crecimiento entre las dos mitades de la secuencia.
            midGrow = if lastL < firstR then 1 else 0
            -- Máximo entre las subsecuencias crecientes: si se encuentra en la primera mitad, en la segunda, o si usa parte de las dos.
            maxSub = maximum [a1, a2, sufL + midGrow + preR]
            pre' = if preL == (lengthS l) - 1 && lastL < firstR then preL + midGrow + preR else preL
            suf' = if sufR == (lengthS r) - 1 && lastL < firstR then sufL + midGrow + sufR else sufR
        in (maxSub, firstL, lastR, pre', suf')


----------------------------------------------
-- b) Definir sccml :: Seq Int -> Int, que resuelva el problema de la "subsecuencia contigua creciente más larga", utilizando scan.

-- Tuppling: (a,b,c,d,e,f)
-- a: Resultado deseado: longitud de la subsecuencia creciente más larga en el segmento.
-- b: primer valor del segmento.
-- c: último valor del segmento.
-- d: longitud del prefijo creciente (desde el inicio del segmento).
-- e: longitud del sufijo creciente (hasta el final del segmento).
-- f: longitud de la subsecuencia actual.
sccmlScan :: Seq s => s Int -> Int
sccmlScan seq = 
    let (_, (maxLong, _, _, _, _, _)) = scanS combine val (mapS base seq)
    in maxLong
        where
            val = (0,maxBound,maxBound,0,0,0)
            base v = (0,v,v,0,0,1)

            combine (a1, firstL, lastL, preL, sufL, lenL) (a2, firstR, lastR, preR, sufR, lenR) = 
                let midGrow = if lastL < firstR then 1 else 0
                    maxSub = maximum [a1, a2, sufL + midGrow + preR]
                    pre' = if preL == lenL - 1 && lastL < firstR then preL + midGrow + preR else preL
                    suf' = if sufR == lenR - 1 && lastL < firstR then sufL + midGrow + sufR else sufR
                    lenTotal = lenL + lenR
                in (maxSub, firstL, lastR, pre', suf', lenTotal)

-- maxBound actúa como un "valor imposible" o "valor neutral" para que la primera comparación que hagas con un elemento real (lastL < firstR) no falle o no provoque resultados erróneos. maxBound es el máximo entero que se puede representar en Haskell.

----------------------------------------------
-- Ejemplos.
seq1 = fromList [9,3,5,1,3,4,5,6,8,1] :: ListSeq Int -- resultado esperado: 5
seq2 = fromList [5,6,2,3,5,1,9] :: ListSeq Int       -- resultado esperado: 2
seq3 = fromList [1,4,6,7,8,11,12,3] :: ListSeq Int   -- resultado esperado: 6
seq4 = fromList [1,2,3,4,5] :: ListSeq Int           -- resultado esperado: 4
seq5 = fromList [5,4,3,2,1] :: ListSeq Int           -- resultado esperado: 0
seq6 = fromList [1] :: ListSeq Int                   -- resultado esperado: 0
seq7 = fromList [] :: ListSeq Int                    -- resultado esperado: 0