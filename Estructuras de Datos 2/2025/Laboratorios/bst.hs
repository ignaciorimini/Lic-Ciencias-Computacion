-- Tipo de datos para árboles binarios de búsqueda.
data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

-- Función que determina si un elemento pertenece a un BST.
member :: Ord a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo l b r) 
    | a == b = True
    | a < b = member a l
    | a > b = member a r

-- Función que devuelve el recorrido inorder de un BST en una lista.
inorder :: Bin a -> [a]
inorder Hoja = []
inorder (Nodo l a r) = inorder l ++ [a] ++ inorder r

-- Función que devuelve el mínimo de un BST (función parcial, no definida para Hojas).
minBST :: Bin a -> a
minBST (Nodo Hoja a r) = a
minBST (Nodo l a r) = minBST l

-- Función que devuelve el máximo de un BST (función parcial, no definida para Hojas).
maxBST :: Bin a -> a
maxBST (Nodo l a Hoja) = a
maxBST (Nodo l a r) = maxBST r

-- Función que chequea si un árbol dado es un BST. Es decir, si cumple las invariantes de la definición de BST.
checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo l a r) = 
    let bstIzq = checkBST l
        bstDer = checkBST r
        invarianteIzq = esHoja l || (maxBST l <= a)
        invarianteDer = esHoja r || (maxBST r > a)
    in bstIzq && bstDer && invarianteIzq && invarianteDer
    where
        esHoja :: Bin a -> Bool
        esHoja Hoja = True
        esHoja _ = False

-- Insertar elemento en un BST (mantener invariantes).
-- Recorremos el árbol hasta encontrar una hoja, que transformamos en un nuevo nodo.
insert :: Ord a => a -> Bin a -> Bin a
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r) 
    | a <= b = (Nodo (insert a l) b r)
    | otherwise = (Nodo l b (insert a r))

-- Borrar elemento de un BST.
delete :: Ord a => a -> Bin a -> Bin a
delete _ Hoja = Hoja
delete x (Nodo l b r)
    | x < b = Nodo (delete x l) b r
    | x > b = Nodo l b (delete x r)
    | otherwise =
        case (l, r) of
            (Hoja, Hoja) -> Hoja
            (Hoja, _) -> r
            (_, Hoja) -> l
            (_, _) -> let y = minBST r
                        in Nodo l y (delete y r)


-- Ejemplos y casos de prueba.
bst1 = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 5 Hoja)) 5 (Nodo Hoja 7 (Nodo Hoja 8 Hoja))
nobst1 = Nodo (Nodo Hoja 2 Hoja) 1 (Nodo Hoja 3 Hoja)