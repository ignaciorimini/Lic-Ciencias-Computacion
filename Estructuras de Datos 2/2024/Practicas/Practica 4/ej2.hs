{-
Definir las siguientes funciones sobre arboles binarios de busqueda (bst):

1. maximum :: Ord a ⇒ BST a → a
Que calcula el maximo valor en un bst.

2. checkBST :: Ord a ⇒ BST a → Bool 
Que chequea si un arbol binario es un bst.

3. splitBST :: Ord a ⇒ BST a → a → (BST a, BST a)
Que dado un arbol bst t y un elemento x , devuelva una tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x

4. join :: Ord a ⇒ BST a → BST a → BST a
Que una los elementos de dos darboles bst en uno.
-}

data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

maxBST :: Ord a => BST a -> a
maxBST (Nodo izq x Hoja) = x
maxBST (Nodo izq x der) = maxBST der

checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo izq x der) =
    let izqValido = chequearMenorQue x der
        derValido = chequearMayorQue x izq
    in izqValido && derValido

chequearMenorQue :: Ord a => a -> BST a -> Bool
chequearMenorQue _ Hoja = True
chequearMenorQue val (Nodo izq x der)
    | val <= x  = True
    | otherwise = False

chequearMayorQue :: Ord a => a -> BST a -> Bool
chequearMayorQue _ Hoja = True
chequearMayorQue val (Nodo izq x der)
    | val > x   = True
    | otherwise = False

splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja _ = (Hoja, Hoja)
splitBST (Nodo izq x der) val
    | x <= val      = let (smaller, larger) = splitBST der val
                        in (Nodo izq x smaller, larger)
    | otherwise     = let (smaller, larger) = splitBST izq val
                        in (smaller, Nodo larger x der)

join :: Ord a => BST a -> BST a -> BST a
join Hoja Hoja = Hoja
join (Nodo izq x der) Hoja = (Nodo izq x der)
join Hoja (Nodo izq x der) = (Nodo izq x der)
join (Nodo izq1 x1 der1) (Nodo izq2 x2 der2)
    | x1 <= x2  = Nodo (join izq1 ) x1 (join der1 (Nodo izq2 x2 der2))
    | otherwise = Nodo izq2 x2 (join der2 (Nodo izq1 x1 der1))

insertar :: Ord a => a -> BST a -> BST a
insertar val Hoja = (Nodo Hoja val Hoja)
insertar val (Nodo izq x der)
    | val <= x  = Nodo (insertar val izq) x der
    | otherwise = Nodo izq x (insertar val der)
