-- Ejemplo:
-- sccml <9, 3, 5, 1, 3, 4, 5, 6, 8, 1> = 5
-- sccml <5, 6, 2, 3, 5, 1, 9> = 2

-- a) sccml utilizando divide & conquer.
-- Dada una secuencia, devuelve el tamaño de la subsecuencia contigua creciente más larga.

data TreeView a t = EMPTY | ELT a | NODE t t
showtS :: Seq a -> TreeView a (Seq a)

sccml :: Seq Int -> Int
sccml s = let (res, _, _) = sccml' s (0, 0, 0)
            in res

-- El segundo argumento representa una tupla con:
-- 1) Resultado deseado.
-- 2) Longitud de la subsecuencia contigua creciente más larga que termina en el último elemento de la izquierda.
-- 3) Longitud de la subsecuencia contigua creciente más larga que comienza en el primer elemento de la derecha.
sccml' :: Seq Int -> Int -> Int
sccml' s acc = case showtS s of
    EMPTY -> acc
    ELT n -> (1, 1, 1)
    NODE l r -> 
        let ((leftRes, leftMaxEnd, leftMaxStart), (rightRes, rightMaxEnd, rightMaxStart)) = sccml' l acc || sccml' r acc
            crossMax = if (lastS l <= firstS r)
                        then leftMaxEnd + rightMaxStart
                        else max leftMaxEnd rightMaxStart
            newLeftMaxEnd = if (lastS l <= firstS r)
                                then leftMaxEnd + rightMaxStart
                                else leftMaxEnd
            newRightMaxStart = if (lastS l <= firstS r)
                                then leftMaxEnd + rightMaxStart
                                else rightMaxStart
            result = max (max leftRes rightRes) crossMax
        in (result, newLeftMaxEnd, newRightMaxStart)

lastS :: Seq a -> a
lastS s = nthS s (length s - 1)

firstS :: Seq a -> a
firstS = nthS s 0


-- Ejemplo: <1, 2, 3, 4> -> N <1, 2> <3, 4>
-- sccml' <1, 2> 0 -> N <1> <2>
--      sccml' <1> = (1, 1, 1)
--      sccml' <2> = (1, 1, 1)
-- crossMax = 1 <= 2 -> 2
-- newLeftMaxEnd = 1 <= 2 -> 2
-- newRightStart = 1 <= 2 -> 2
-- result = max 1 1 2 = 2
-- -> (2, 2, 2)

-- sccml' <3, 4> 0 -> (2, 2, 2)
-- crossMax = 2 <= 3 -> 4
-- newLeftMaxEnd = 4
-- newRightStart = 4
-- result = max 2 2 4 = 4
-- -> (4, 4, 4)

----------------------------------------
-- b) sccml utilizando scan.
sccml :: Seq Int -> Int
sccml s = 
    let (increasingLenghts, _) = scan updateFunc (0, minBound) s
    in reduce max 0 increasingLenghts
    where
        updateFunc :: (Int, Int) -> Int -> (Int, Int)
        updateFunc (len, prev) curr
            | curr > prev = (len + 1, curr)
            | otherwise = (1, curr)