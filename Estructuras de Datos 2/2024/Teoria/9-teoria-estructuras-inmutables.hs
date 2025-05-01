{-
ESTRUCTURAS INMMUTABLES

-----------------------
ESTRUCTURAS DE DATOS FUNCIONALES VS IMPERATIVAS
Muchos de los algoritmos tradicionales están pensados para estructuras efímeras. 

Las estructuras efímeras soportan una sola version y son coherentes con un modelo secuencial. Los cambios en estas estructuras son destructivos.

Las estructuras inmutables, soportan varias versiones y son más fácilmente paralelizables. Esta flexibilidad tiene un costo:
- Debemos adaptar las estructuras y algoritmos al modelo inmutable.
- Hay ciertas cotas de las estructuras efímeras que no siempre se van a poder alcanzar.


-----------------------
INMUTABILIDAD Y SHARING
Las estructuras inmutables son estructuras de datos cuyos valores no pueden ser modificados una vez que han sido creados. En programación funcional, la inmutabilidad es una característica fundamental, donde los datos se tratan como valores constantes que no cambian con el tiempo. En lugar de modificar los datos existentes, se crean nuevos datos basados en los datos originales.

En lenguaje funcional puro, todas las estructuras son inmutables. Estas, no se destruyen al hacer un cambio, más bien se copian los datos y se modifica la copia.

Los nodos de una lista enlazada que no cambian, pueden ser compartidos por las diferentes versiones (sharing). Notar que el manejo automático de la memoria (garbage collection) es prácticamente esencial.


-----------------------
LISTAS ENLAZADAS SIMPLE
La concatenación de listas enlazadas simples efímeras zs = xs ++ ys destruye las listas xs e ys pero la operación es O(1).

En cambio, la concatenación de listas enlazadas simples Inmutables zs = xs ++ ys no destruye las listas, sino que copia xs y hace que el último nodo apunte al primero de ys.

Así, luego de la concatenación tendremos las tres listas xs, ys y zs, donde zs se obtuvo de copiar los nodos de xs, pero comparte los nodos de ys.

La operación de copiar todos los nodos de xs en zs es O(|xs|).


-----------------------
LISTAS EN HASKELL

Función que modifica un solo elemento de la lista.
update :: [a] -> Int -> a -> [a]
update [] _ _ = []
update (x:xs) 0 x' = x' : xs
update (x:xs) i x' = x : update xs (i-1) x'


-----------------------
ÁRBOLES BINARIOS EN HASKELL
Un árbol binario es un árbol en el que cada nodo tiene exactamente dos hijos. En Haskell representamos un arbol binario con la siguiente definición recursiva:
data Bin a = Hoja | Nodo (Bin a) a (Bin a)

Definimos funciones sobre los árboles mediante pattern-matching y recursión:
member :: Eq a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo I b r) = (a == b) || member a I || member a r

Si el árbol no está balanceado y tiene una única rama, la complejidad sería O(n), pues en el peor de los casos, habría que recorrer todos los nodos del árbol para determinar si un elemento dado está presente.

Las operaciones de búsqueda, inserción y borrado son del orden de la altura del árbol. En el mejor caso son O(log n), pero en el peor caso pueden ser O(n) (por ejemplo, al insertar datos ordenados, el árbol degenera en una lista).

La solución es mantener el árbol balanceado, por ejemplo, utilizando un AVL o un Red-Black Tree.
-}