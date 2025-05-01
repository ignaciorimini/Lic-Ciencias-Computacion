{-
3) Asumiendo que A es un tipo con igualdad, completar la siguiente especificación del TAD conjunto.

tad Conjunto (A:Set) where
    vacio : Conjunto A
    insertar : A -> Conjunto A -> Conjunto A
    borrar : A -> Conjunto A -> Conjunto A
    esVacio : Conjunto A -> Bool
    union : Conjunto A -> Conjunto A -> Conjunto A
    interseccion : Conjunto A -> Conjunto A -> Conjunto A
    resta : Conjunto A -> Conjunto A -> Conjunto A

insertar x (insertar x c) = insertar x c
insertar x (insertar y c) = insertar y (insertar x c)

borrar x (insertar x vacio) = vacio
borrar x (insertar x c) = c

esVacio vacio = True
esVacio (insertar x c) = False

union vacio vacio = True
union vacio (insertar x c) = insertar x c
union (insertar x c) vacio = insertar x c
union (insertar x c) (insertar y d) = 
    if x == y 
        then union (insertar x c) d 
        else union (insertar x (insertar y vacio)) (union c d)

interseccion vacio (insertar x c) = vacio
interseccion (insertar x c) vacio = vacio
interseccion (insertar x c) (insertar y d) =
    if x == y
        then union (insertar x vacio) (interseccion c d)  
        else union (insertar x (insertar y vacio)) (interseccion c d)

resta vacio vacio = vacio
resta (insertar x c) vacio = insertar x c
resta vacio (insertar x c) = vacio
resta (insertar x c) (insertar y d) =
    if x == y
        then resta c d
        else resta (insertar x c) d


_______________________________
b) ¿Qué pasaría si se agregase una función 
choose : Conjunto A -> A tq choose (insertar x c) = x

Como insertar x (insertar y c) = insertar y (insertar x c), es decir, no hay un orden en el conjunto, entonces choose no puede ser implementada.

choose (insertar x c) = <regla 1>
choose (insertar x (insertar x c)) = <regla 2>
choose (insertar x (insertar x c))

-}