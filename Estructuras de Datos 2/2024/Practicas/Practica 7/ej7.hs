-- a) Función merge.
-- Dadas una relación de orden y dos secuencias ordenadas (respecto a esta relación) s1 y s2, construye una secuencia ordenada con los elementos de s1 y s2. La función debe utilizar Divide & Conquer.

merge :: (a -> a -> Ordering) -> Seq a -> Seq a -> Seq a
merge cmp s1 s2
    | emptyS s1 = s2
    | emptyS s2 = s1
    | otherwise = if cmp (firstS s1) (firstS s2) == LT --s1 <= s2
                    then cons (firstS s1) (merge (drop 1 s1) s2)
                    else cons (firstS s2) (merge s1 (drop 1 s2))


-- b) Función sort.
-- Ordena una secuencia según una relación de orden dada.
sort :: (a -> a -> Ordering) -> Seq a -> Seq a
sort cmp s = case showtS s of
    EMPTY -> empty
    ELT n -> singleton n
    NODE l r -> let (l', r') = sort cmp l || sort cmp r
                in merge cmp l r


-- c) Función maxE.
-- Devuelve el máximo de una secuencia.
maxE :: (a -> a -> Ordering) -> Seq a -> a
maxE cmp s = 
    | isEmpty s = error "Empty seq has no maximum."
    | otherwise =
        let (_, result) = scan maxCmp (first s) s
        in result
        where
            maxCmp :: a -> a -> a
            maxCmp x y = if cmp x y == LT
                            then y
                            else x


-- d) Función maxS.
-- Devuelve el índice de un máximo en la secuencia.
maxS :: (a -> a -> Ordering) -> Seq a -> Nat
maxS cmp s
    | isEmpty s = error "Empty seq has no maximum."
    | otherwise = 
        let maximum = maxE cmp s
        in search cmp maximum 0 s
        where
            search :: (a -> a -> Ordering) -> a -> Nat -> Seq a -> Bool
            search cmp x i s = 
                if cmp x (first s) == EQ
                    then i
                    else search cmp x (i + 1) (drop 1 s)


-- e) Función group.
-- Dada una secuencia, agrupa los elementos iguales contiguos.
-- Por ejemplo, group <1, 1, 2, 3, 4, 4, 2, 2> = <1, 2, 3, 4, 2>
group :: (a -> a -> Ordering) -> Seq a -> Seq a
group cmp s
    | isEmptyy s = empty
    | otherwise = foldr addIfDifferent empty (reverse s)
    where
        addIfDifferent x acc
            | isEmpty acc = cons x acc
            | otherwise = if cmp x (first acc) == EQ
                            then acc
                            else cons x acc

-- foldr = addIfDifferent s0 (add s1 ... (add sn empty))


-- f) Función collect.
-- Recolecta todos los datos asociados a cada clave y devuelve una secuencia de pares ordenados según el primer elemento.
-- Por ejemplo: collect <(2, "A"), (1, "B"), (1, "C"), (2, "D")> = <(1, ("B", "C"), (2, ("A", "D")))
collect :: Seq (a, b) -> Seq (a, Seq b)
collect s = 
    let sorted = sort compareKeys s
        grouped = group sameKey sorted
    in map toKeyValuePair grouped
    where
        compareKeys (a1, _) (a2, _) = a1 <= a2
        sameKey (a1, _) (a2, _) = a1 == a2

        toKeyValuePair :: Seq (a, b) -> Seq (a, Seq b)
        toKeyValuePair s = ((first s), map second s)