data Color = R | B deriving (Eq, Show)
data AATree a = N Color a (AATree a) (AATree a) | E deriving (Eq, Show)

-- Función que dado un valor de tipo AATree, determina si es un árbol binario de búsqueda.
-- isBST = O(n log(n))
-- isBST :: Ord a => AATree a -> Bool
-- isBST E = True
-- isBST (N c v i r) =
--     let izqBST = isBST i
--         derBST = isBST r
--         izqValido = case (maximum' i) of
--                         Nothing -> True
--                         Just maxL -> maxL <= v
--         derValido = case (minimum' r) of
--                         Nothing -> True
--                         Just minR -> minR > v
--     in izqBST && derBST && izqValido && derValido
--     where
--         maximum' :: Ord a => AATree a -> Maybe a
--         maximum' E = Nothing
--         maximum' (N _ v _ E) = Just v
--         maximum' (N _ v _ r) = maximum' r

--         minimum' :: Ord a => AATree a -> Maybe a
--         minimum' E = Nothing
--         minimum' (N _ v E _) = Just v
--         minimum' (N _ v i _) = minimum' i


-- isBST = O(n), es más eficiente esta función.
-- Idea: pasamos el mínimo y máximo valido en cada árbol. Al analizar el subárbol derecho, todos sus elementos deben ser menores que el máximo válido, que es el valor del nodo actual.
isBST :: Ord a => AATree a -> Bool
isBST t = isBST' t Nothing Nothing

isBST' :: Ord a => AATree a -> Maybe a -> Maybe a -> Bool
isBST' E _ _ = True
isBST' (N _ v l r) minVal maxVal =
    let izqValido = case minVal of
                        Nothing -> True
                        Just minV -> v > minV
        derValido = case maxVal of
                        Nothing -> True
                        Just maxV -> v < maxV
    in izqValido && derValido && isBST' l minVal (Just v) && isBST' r (Just v) maxVal


-- Dado un AATree y un elemento, determina si el elemento está en el árbol.
member :: Ord a => a -> AATree a -> Bool
member _ E = False
member x (N c v i r)
    | x == v = True
    | x < v = member x i
    | otherwise = member x r


-- Dado un valor de tipo AATree, determina si es un AATree (cumple con las invariantes).
-- isAATree = O(n)
isAATree :: Ord a => AATree a -> Bool
isAATree E = True
isAATree t@(N c v i r) =
    let raizNegra = isRootBlack t
        esBinario = isBST t             -- O(n)
        invariante1 = checkInv1 t       -- O(n)
        invariante2 = checkInv2 t       -- O(n)
        invariante3 = checkInv3 t       -- O(n)
    in raizNegra && esBinario && invariante1 && invariante2 && invariante3
    where
        isRootBlack :: AATree a -> Bool
        isRootBlack (N B _ _ _) = True
        isRootBlack _ = False

        isRootRed :: AATree a -> Bool
        isRootRed (N R _ _ _) = True
        isRootRed _ = False

        alturaNegra :: AATree a -> Int
        alturaNegra E = 0
        alturaNegra t@(N c _ i r) =
            let izqAlt = alturaNegra i
                derAlt = alturaNegra r
                currentAlt = if isRootBlack t then 1 else 0
            in if (izqAlt == derAlt) && (izqAlt /= -1)
                then izqAlt + currentAlt
                else -1

        checkInv1 :: AATree a -> Bool
        checkInv1 E = True
        checkInv1 (N B _ i r) = checkInv1 i && checkInv1 r
        checkInv1 (N R _ i r) =
            let izqRojo = isRootRed i
                derRojo = isRootRed r
                actualValido = (not izqRojo) && (not derRojo)
            in actualValido && checkInv1 i && checkInv1 r

        checkInv2 :: AATree a -> Bool
        checkInv2 E = True
        checkInv2 (N _ _ E _) = True
        checkInv2 (N _ _ i r) = (not (isRootRed i)) && checkInv2 i && checkInv2 r
        
        checkInv3 :: AATree a -> Bool
        checkInv3 E = True
        checkInv3 t = (alturaNegra t) /= -1


insert :: Ord a => a -> AATree a -> AATree a
insert x t = makeBlack (ins x t)
    where
        makeBlack :: AATree a -> AATree a
        makeBlack (N _ v i r) = N B v i r
        makeBlack t = t

        ins :: Ord a => a -> AATree a -> AATree a
        ins x E = N R x E E
        ins x t@(N c v i r)
            | x == v = t
            | x < v = balance (N c v (ins x i) r)
            | otherwise = balance (N c v i (ins x r))

        balance :: AATree a -> AATree a
        balance = split . skew

        skew :: AATree a -> AATree a
        skew (N color y (N R x a b) c) = N color x a (N R y b c)
        skew t = t

        split :: AATree a -> AATree a
        split (N B x a (N R y b (N R z c d))) = N R y (N B x a b) (N B z c d)
        split t = t



