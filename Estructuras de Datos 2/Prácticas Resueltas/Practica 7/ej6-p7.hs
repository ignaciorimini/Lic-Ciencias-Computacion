import Seq
import ListSeq

-- Dada una secuencia de enteros s, se desea determinar el tamaño del siguiente conjunto:
-- multiplos(s) = {(i,j) | 0 <= i < j < |s|, mod(s_i, s_j) == 0}

-- Ejemplos.
-- multiplos <12,4,6,3,2> tiene tamaño 7.
-- multiplos <4,6,2> tiene tamaño 2.
-- multiplos <1,2,3,4,5> tiene tamaño 0.

-- Dada una secuencia s, calcula el tamaño de multiplos(s). Definir en términos de operaciones paralelizables, por ejemplo usando map y reduce.
cantMultiplos :: Seq s => s Int -> Int
cantMultiplos seq =                                         -- <4,6,2>
    let n = lengthS seq                                     -- n = 3

        seqIndices :: ListSeq Int
        seqIndices = tabulateS id n                         -- <0,1,2>

        paresIJ :: ListSeq (ListSeq (Int, Int))
        paresIJ = mapS (funAuxPares seqIndices) seqIndices  -- <<(0,1),(0,2)>, <(1,2)>, <>>

        paresIJPlano :: ListSeq (Int, Int)
        paresIJPlano = flattenS paresIJ                     -- <(0,1),(0,2),(1,2)>

        bools :: ListSeq Bool
        bools = mapS (funAuxMod seq) paresIJPlano           -- <False,True,True>

        onlyTrue :: ListSeq Bool
        onlyTrue = filterS (== True) bools                  -- <True, True>
    in lengthS onlyTrue
    where
        funAuxPares seqIndices i = mapS (\j -> (i,j)) (dropS (i+1) seqIndices)

        funAuxMod seq (i,j) =
            let a = nthS seq i
                b = nthS seq j
            in mod a b == 0

----------------------------------------------
-- Ejemplos.
seq1 = fromList [12,4,6,3,2] :: ListSeq Int -- resultado esperado: 7
seq2 = fromList [4,6,2] :: ListSeq Int      -- resultado esperado: 2
seq3 = fromList [1,2,3,4,5] :: ListSeq Int  -- resultado esperado: 0