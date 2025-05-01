{-
-----------------------
ÁRBOLES BINARIOS DE BÚSQUEDA EN HASKELL
Un árbol binario de búsqueda es un árbol binario t tal que
- o bien t es una hoja
- o bien t es un Nodo I a r, y se cumple que:
    + I y r son árboles binarios de búsqueda
    + si y es una clave en algún nodo de I entonces y <= a
    + si y es una clave en algún nodo de r entonces a < y

En las funciones member, minimum y maximum solo recorremos (a lo sumo) un camino entre la raíz y una hoja. Esto da lugar al siguiente teorema:
Teorema: las operaciones member, minimum y maximum son O(h), donde h es la áltura del árbol.

Al insertar en un BST, ocurre un Sharing. Se duplican los nodos de la rama que va desde la raíz hasta el nodo insertado y se comparten los restantes nodos con Sharing.
-}


data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show
-- Para crear un arbol binario:
-- arbol = Nodo (Nodo Hoja 1 Hoja) 2 (Nodo Hoja 3 Hoja)


--------------------------------
-- Función que determina si un elemento pertenece a un BST.
member :: Ord a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo i b r) 
    | a == b    = True
    | a < b     = member a i
    | a > b     = member a r


--------------------------------
-- Función que recorre el BST de forma inorder y devuelve sus valores en una lista.
inorder :: Bin a -> [a]
inorder Hoja = []
inorder (Nodo i a r) = inorder i ++ [a] ++ inorder r


--------------------------------
-- Función que determina el mínimo valor de un BST.
minBST :: Bin a -> a
minBST (Nodo Hoja a r) = a
minBST (Nodo i a r) = minBST i


--------------------------------
-- Función que determina el máximo valor de un BST.
maxBST :: Bin a -> a
maxBST (Nodo i a Hoja) = a
maxBST (Nodo i a r) = maxBST r


--------------------------------
-- Función para verificar si un árbol binario es un BST
checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True  -- La hoja es un BST válida
checkBST (Nodo izq x der) =
    let izqValido = checkMenorQue x izq
        derValido = checkMayorQue x der
    in izqValido && derValido

-- Función para verificar si todos los valores en el árbol son menores que un valor dado
checkMenorQue :: Ord a => a -> Bin a -> Bool
checkMenorQue _ Hoja = True
checkMenorQue x (Nodo izq y der) =
    x > y && checkMenorQue x izq && checkMenorQue x der

-- Función para verificar si todos los valores en el árbol son mayores que un valor dado
checkMayorQue :: Ord a => a -> Bin a -> Bool
checkMayorQue _ Hoja = True
checkMayorQue x (Nodo izq y der) =
    x < y && checkMayorQue x izq && checkMayorQue x der


--------------------------------
-- Función para insertar un elemento en un BST.
insert :: Ord a => a -> Bin a -> Bin a
insert val Hoja = Nodo Hoja val Hoja
insert val (Nodo izq x der)
    | val <= x  = Nodo (insert val izq) x der
    | otherwise = Nodo izq x (insert val der)


--------------------------------
-- Función para borrar un elemento en un BST.
delete :: Ord a => a -> Bin a -> Bin a
delete _ Hoja = Hoja
delete val (Nodo izq x der)     | val < x = Nodo (delete val izq) x der
delete val (Nodo izq x der)     | val > x = Nodo izq x (delete val der)
delete val (Nodo Hoja x Hoja)   | val == x = Hoja
delete val (Nodo izq x Hoja)    | val == x = izq
delete val (Nodo Hoja x der)    | val == x = der
delete val (Nodo izq x der)     | val == x =
    let y = minBST der
    in Nodo izq y (delete y der)
