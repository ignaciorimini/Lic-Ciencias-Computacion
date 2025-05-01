data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

maximum' :: Ord a => BST a -> a
maximum' (Nodo _ v Hoja) = v
maximum' (Nodo i v r) = maximum' r

checkBST :: Ord a => BST a -> Bool
checkBST t@(Nodo i v r) = checkBST' t Nothing Nothing
    where
        checkBST' :: Ord a => BST a -> Maybe a -> Maybe a -> Bool
        checkBST' Hoja _ _ = True
        checkBST' (Nodo i v r) minVal maxVal =
            let izqValido = case minVal of
                            Nothing -> True
                            Just minV -> v > minV
                derValido = case maxVal of
                            Nothing -> True
                            Just maxV -> v < maxV
            in izqValido && derValido && checkBST' i minVal (Just v) && checkBST' r (Just v) maxVal


splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja _ = (Hoja, Hoja)
splitBST (Nodo izq x der) val
    | x <= val = 
        let (smaller, larger) = splitBST der val
        in (Nodo izq x smaller, larger)
    | otherwise = 
        let (smaller, larger) = splitBST izq val
        in (smaller, Nodo larger x der)


join :: Ord a => BST a -> BST a -> BST a
join Hoja t = t
join t Hoja = t
join t1@(Nodo i1 v1 r1) t2@(Nodo i2 v2 r2)
    | v1 < 