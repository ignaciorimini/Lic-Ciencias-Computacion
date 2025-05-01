data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

-- Función que devuelve el recorrido inorder de un bst.
inorder :: Ord a => BST a -> [a]
inorder Hoja = []
inorder (Nodo l a r) = (inorder l) ++ [a] ++ (inorder r)

-- Función que calcula el máximo valor en un bst.
maximumBST :: Ord a => BST a -> a
maximumBST (Nodo l a Hoja) = a
maximumBST (Nodo l a r) = maximumBST r

-- Función que chequea si un árbol binario es un bst.
checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo l a r) = 
    let izqBST = checkBST l
        derBST = checkBST r
        invIzq = esHoja l || (maximumBST l <= a)
        invDer = esHoja r || (maximumBST r > a)
    in izqBST && derBST && invIzq && invDer
    where
        esHoja :: BST a -> Bool
        esHoja Hoja = True
        esHoja _ = False

-- Función que dado un árbol bst t y un elemento x, devuelve una tupla con un bst con los elementos de t menores iguales a x y un bst con los elementos de t mayores a x.
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja _ = (Hoja, Hoja)
splitBST (Nodo l a r) x
    | a <= x = 
        let (smaller, larger) = splitBST r x
        in (Nodo l a smaller, larger)
    | otherwise = 
        let (smaller, larger) = splitBST l x
        in (smaller, Nodo larger a r)

-- Función que una los elementos de dos árboles bst en uno.
join :: Ord a => BST a -> BST a -> BST a
join Hoja t = t
join t Hoja = t
join t1@(Nodo l1 a1 r1) t2@(Nodo l2 a2 r2) = 
    let (smaller, larger) = splitBST t2 a1
    in Nodo (join l1 smaller) a1 (join r1 larger)

-- Ejemplos y casos de prueba.
bst1 = Nodo (Nodo Hoja 4 Hoja) 6 (Nodo (Nodo Hoja 8 Hoja) 10 (Nodo Hoja 12 Hoja))
bst2 = Nodo (Nodo Hoja 1 Hoja) 2 (Nodo Hoja 3 Hoja)
-- Probar checkBST (join bst1 bst2)
-- Probar inorder (join bst1 bst2)