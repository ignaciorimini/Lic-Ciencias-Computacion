import Seq
import ListSeq

-- Dadas una relación de orden y dos secuencias ordenadas (respecto a ésta relación) s1 y s2, construye una secuencia ordenada con los elementos de s1 y s2. Definir usando Divide & Conquer
mergeS :: Seq s => (a -> a -> Ordering) -> s a -> s a -> s a
mergeS cmp seq1 seq2 = case (showtS seq1, showtS seq2) of
    (EMPTY, _) -> seq2
    (_, EMPTY) -> seq1
    (ELT x, ELT y) ->
        if cmp x y /= GT -- x <= y
            then appendS seq1 seq2 
            else appendS seq2 seq1
    (NODE l1 r1, _) ->
        let x = firstS r1
            (l2, r2) = splitAtS cmp seq2 x
            (l', r') = (mergeS cmp l1 l2, mergeS cmp r1 r2)
        in appendS l' r'
    (_, NODE _ _) -> mergeS cmp seq2 seq1 -- simétrico

splitAtS :: Seq s => (a -> a -> Ordering) -> s a -> a -> (s a, s a)
splitAtS cmp s x = case showtS s of
    EMPTY -> (emptyS, emptyS)
    ELT y ->
        if cmp y x == LT
            then (singletonS y, emptyS)
            else (emptyS, singletonS y)
    NODE l r ->
        let (l1, l2) = splitAtS cmp l x
            (r1, r2) = splitAtS cmp r x
        in (appendS l1 r1, appendS l2 r2)


----------------------------------------------
-- Ordena una secuencia según una relación de orden dada.
sortS :: Seq s => (a -> a -> Ordering) -> s a -> s a
sortS cmp seq = case showtS seq of
    EMPTY -> seq
    ELT v -> seq
    NODE l r ->
        let (leftSorted, rightSorted) = (sortS cmp l, sortS cmp r)
        in mergeS cmp leftSorted rightSorted


----------------------------------------------
-- Devuelve el máximo de una secuencia: usando Divide & Conquer.
maxE :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE cmp seq = case showtS seq of
    EMPTY -> error "maxE: secuencia vacía"
    ELT x -> x
    NODE l r ->
        let (maxL, maxR) = (maxE cmp l, maxE cmp r)
        in if cmp maxL maxR == GT then maxL else maxR

-- Devuelve el máximo de una secuencia: usando reduce.
maxE' :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE' cmp seq = reduceS maxBy (firstS seq) seq
    where
        maxBy x y = if cmp x y == GT then x else y


----------------------------------------------
-- Devuelve el índice de un máximo en la secuencia.
maxS :: Seq s => (a -> a -> Ordering) -> s a -> Int
maxS cmp seq = 
    let n = lengthS seq
        seqIndices = tabulateS id n
        seqValorIndice = zipS seq seqIndices
        (maxVal, maxIndice) = reduceS funAux (firstS seqValorIndice) seqValorIndice
    in maxIndice
    where
        funAux (x, i1) (y, i2) = 
            if cmp x y == GT
            then (x, i1)
            else (y, i2)

----------------------------------------------
-- Recolecta todos los datos asociados a cada clave y devuelve una secuencia de pares ordenada según el primer elemento.
collectS :: Seq s => s (a,b) -> s (a, s b)
collectS seq =
    let seqOrdenada = sortS (\(k1, _) (k2, _) -> compare k1 k2) seq
    in agruparS seqOrdenada
    where
        agrupar s = case showlS s of
            NIL -> emptyS
            CONS (k, v) resto ->
                let (mismos, restantes) = takeIguales k resto
                    valores = consS v mismos
                in consS (k, valores) (agrupar restantes)

        takeIguales k s = case showlS s of
            NIL -> (emptyS, emptyS)
            CONS (k', v') s' ->
                if k == k'
                then let (vs, rest) = takeIguales k s'
                    in (consS v' vs, rest)
                else (emptyS, s)



----------------------------------------------
-- Ejemplos.
s1 = fromList [1,3,5] :: ListSeq Int
s2 = fromList [2,4,6] :: ListSeq Int
s3 = fromList [4] :: ListSeq Int
s4 = fromList [10,9,8,7,6,5,4] :: ListSeq Int