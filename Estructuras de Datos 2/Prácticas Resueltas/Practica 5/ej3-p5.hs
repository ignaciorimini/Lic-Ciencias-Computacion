{-
3)
tad Conjunto (A:Set) where
    import Bool
    vacio : Conjunto A
    insertar : A -> Conjunto A -> Conjunto A
    borrar : A -> Conjunto A -> Conjunto A
    esVacio : Conjunto A -> Bool
    union : Conjunto A -> Conjunto A -> Conjunto A
    interseccion : Conjunto A -> Conjunto A -> Conjunto A
    resta : Conjunto A -> Conjunto A -> Conjunto A

___________________________________________________
a) Asumiendo que A es un tipo con igualdad, completar la especificación algebraica del TAD conjunto.

insertar x (insertar x c) = insertar x c
insertar x (insertar y c) = insertar y (insertar x c)

borrar x vacio = vacio
borrar x (insertar y c) = if (x == y) then (borrar x c) else (insertar y (borrar x c))

esVacio vacio = True
esVacio (insertar x c) = False

union vacio c = c
union c vacio = c
union (insertar x c) s = insertar x (union c (borrar x s))

interseccion vacio c = vacio
interseccion c vacio = vacio
interseccion (insertar x c) s =
    if x ∈ s then (insertar x (interseccion c (borrar x s)))
    else (interseccion c s)

resta vacio c = vacio
resta c vacio = c
resta (insertar x c) s = 
    if x ∈ s then (resta c s)
    else (insertar x (resta c s))

-- ∈ se define como (elem x s)

___________________________________________________
b) ¿Qué pasaría si se agregase una función
choose : Conjunto A -> A tal que choose 
(insertar x c) = x?

Si agregamos la función choose con la especificación algebraica dada, surgirían varios problemas teóricos y prácticos:

- El orden de inserción no debe afectar al contenido, pero con choose, el resultado depende del último elemento insertado. Esto viola el principio de extensionalidad: dos conjuntos iguales deben comportarse igual en todas las operaciones.

- La especificación además es parcial, pues no especifica que se hace con el conjunto vacío.
-}