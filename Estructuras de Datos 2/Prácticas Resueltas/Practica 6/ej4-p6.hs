data T a = E | N (T a) a (T a) deriving Show

altura :: T a -> Int
altura E = 0
altura (N l x r) = 1 + max (altura l) (altura r)

----------------------------------------------
-- Sean t1, t2 :: T a, la función combinar debe satisfacer la siguiente especificación:
-- combinar t1 t2 contiene todos los elementos de t1 y t2 y no contiene ningún otro elemento.
-- altura (combinar t1 t2) <= 1 + max (altura t1) (altura t2)
-- Wcombinar(d1, d2), Scombinar(d1, d2) deben ser O(d1), donde d1 y d2 son las alturas de los árboles t1 y t2 que recibe como argumento la función combinar.
combinar :: T a -> T a -> T a
combinar t1 t2 = insertarLista (inorder t1) t2

-- Devuelve una lista de los elementos del árbol usando recorrido inorder.
inorder :: T a -> [a]
inorder E = []
inorder (N l x r) = inorder l ++ [x] ++ inorder r
        
-- Inserta los elementos de una lista en un árbol.
insertarLista :: [a] -> T a -> T a
insertarLista [] t = t
insertarLista (x:xs) t = insertarLista xs (insertar x t)

-- Dado un elemento y un árbol, inserta el elemento en el árbol de forma balanceada (lo inserta en el subárbol con menor altura).
insertar :: a -> T a -> T a
insertar x E = N E x E
insertar x (N l y r)
    | altura l <= altura r = N (insertar x l) y r
    | otherwise = N l y (insertar x r)


----------------------------------------------
-- Similar a la función filter sobre listas. 
-- Sean p :: a -> Bool y t :: T a  debe satisfacer la siguiente especificación:
-- filterT p t contiene todos los elementos de t que satisfacen p y no contiene ningún otro elemento.
-- altura (filterT p t) <= altura t
-- SfilterT(d) = O(d^2), donde d es la altura del árbol.
filterT :: (a -> Bool) -> T a -> T a
filterT _ E = E
filterT p (N l x r) =
    if p x 
        then N (filterT p l) x (filterT p r) 
        else combinar (filterT p l) (filterT p r)


----------------------------------------------
-- Esta función debe implementar el algoritmo quicksort sobre árboles y debe utilizar en la definición las funciones anteriores.
-- Sea t : T Int, quicksort t es un árbol binario de búsqueda que contiene todos los elementos de t y ningún otro elemento.
quicksortT :: T Int -> T Int
quicksortT E = E
quicksortT t@(N l x r) =
    let (arbolMenores, arbolMayores) = (filterT (<= x) (combinar l r), filterT (> x) (combinar l r))
        (izqOrd, derOrd) = (quicksortT arbolMenores, quicksortT arbolMayores)
    in N izqOrd x derOrd
        

{-
a. Calcular WquicksortT(n) en el peor caso, siendo n la cantidad de nodos del árbol que recibe como argumento la función.

WquicksortT(n) = 2 * WquicksortT(Wfilter(Wcombinar(n-1, n-1)))
               = 2 * WquicksortT(Wfilter(d1))
               = 2 * WquicksortT(d^2)

b. Suponiendo que quicksortT recibe un árbol balanceado, calcular el trabajo y la profunidad de la función en el peor caso, mejor caso y suponiendo que el pivote divide a los datos en proporción 1 a 9. ¿Qué cambiaría en el último caso si la proporción es 1 a 99?
-}

-- Ejemplos.
t1, t2 :: T Int
t1 = N (N (N E 50 E) 21 (N E 2 E)) 10 (N (N E 44 E) 14 (N E 3 E))
t2 = N (N E 6 E) 8 (N E 9 E)
-- inorder t1 -> [50,21,2,10,44,14,3]
-- inorder (quicksortT t1) -> [2,3,10,14,21,44,50]
-- inorder (quicksortT (combinar t1 t2)) -> [2,3,6,8,9,10,14,21,44,50]