data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

-- Función que dado un elemento y un árbol BST, devuelve True si el elemento pertenece al árbol, False en caso contrario.
member :: Ord a => a -> Bin a -> Bool
member _ Hoja = False
member x (Nodo l v r)
    | x == v = True
    | x <= v = member x l
    | otherwise = member x r


-- Función que dado un BST, devuelve una lista ordenada de forma "in order" con sus elementos.
inorder :: Bin a -> [a]
inorder Hoja = []
inorder (Nodo l v r) =
    let izqOrdenado = inorder l
        derOrdenado = inorder r
    in izqOrdenado ++ [v] ++ derOrdenado


-- Función que dado un BST, devuelve el mínimo elemento del mismo.
minimum' :: Bin a -> a
minimum' (Nodo Hoja v r) = v
minimum' (Nodo l v r) = minimum' l


-- Función que dado un BST, devuelve el máximo elemento del mismo.
maximum' :: Bin a -> a
maximum' (Nodo l v Hoja) = v
maximum' (Nodo l v r) = maximum' r


-- Función que dado un elemento y un BST, inserta el elemento al árbol.
insert :: Ord a => a -> Bin a -> Bin a
insert x Hoja = Nodo Hoja x Hoja
insert x t@(Nodo l v r) 
    | x == v = t
    | x <= v = rebalance (Nodo (insert x l) v r)
    | otherwise = rebalance (Nodo l v (insert x r))


-- Función que dado un BST, devuelve su altura.
altura :: Bin a -> Int
altura Hoja = 0
altura (Nodo l v r) = 1 + max (altura l) (altura r)


-- Función que dado un árbol, lo rebalancea.
rebalance :: Ord a => Bin a -> Bin a
rebalance Hoja = Hoja
rebalance t@(Nodo l v r)
    | altura l > altura r + 1 = rebalance (rotacionDer t)
    | altura r > altura l + 1 = rebalance (rotacionIzq t)
    | otherwise = t



-- Función que dado un árbol BST, aplica una rotación hacia la izquierda.
rotacionIzq :: Bin a -> Bin a
rotacionIzq (Nodo l v (Nodo rl rv rr)) = Nodo (Nodo l v rl) rv rr
rotacionIzq t = t


-- Función que dado un árbol BST, aplica una rotación hacia la derecha.
rotacionDer :: Bin a -> Bin a
rotacionDer (Nodo (Nodo ll lv lr) v r) = Nodo ll lv (Nodo lr v r)
rotacionDer t = t


-- Función que dada una lista, devuelve un BST.
toTree :: Ord a => [a] -> Bin a
toTree [] = Hoja
toTree (x:xs) = insert x (toTree xs)


-- Función que dado un elemento y un árbol BST, lo elimina del mismo.
delete :: Ord a => a -> Bin a -> Bin a
delete _ Hoja = Hoja
delete x t@(Nodo Hoja v Hoja) = if x == v then Hoja else t
delete x t@(Nodo l v r)
    | x == v =
        let newval = minimum' r
        in rebalance (Nodo l newval (delete newval r))
    | x < v = (Nodo (delete x l) v r)
    | otherwise = (Nodo l v (delete x r))


-- Función para chequear si un árbol es un BST o no.
checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo l v r) =
    let izqBST = checkBST l
        derBST = checkBST r
        izqValido = checkMenorQue l v
        derValido = checkMayorQue r v
    in izqBST && derBST && izqValido && derValido
    where
        checkMenorQue Hoja x = True
        checkMenorQue (Nodo l v r) x
            | v <= x = (checkMenorQue l x) && (checkMenorQue r x)
            | otherwise = False

        checkMayorQue Hoja x = True
        checkMayorQue (Nodo l v r) x
            | v >= x = (checkMayorQue l x) && (checkMayorQue r x)
            | otherwise = False