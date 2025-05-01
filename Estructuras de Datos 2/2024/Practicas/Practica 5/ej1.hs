{-
1)
Operaciones
tad List A where
    nil : List A
    cons : A -> List A -> List A
    null : List A -> Bool
    head : List A -> A
    tail : List A -> List A

___________________________
a. Dar una especificación algebraica del TAD de listas finitas.
null nil = True
null (cons x nil) = False
head (cons x nil) = x
tail (cons x xs) = xs

___________________________
b. Dar una especificación tomando el modelo de las secuencias <x1, ... xn>
nil = <>
cons x <> = <x>
cons x <x1, x2, ... xn> = <x, x1, x2, ... xn>
head <x1, x2, ... xn> = x1
tail <x1, x2, ... xn> = <x2, x3, ... xn>
null <x1, x2, ... xn> = True si n = 0
null <x1, x2, ... xn> = False en otro caso

___________________________
c. Asumiendo que A es un tipo con igualdad, especificar una función inL : List A -> A -> Bool tal que inL ls x = true sii x es elemento de ls.

inL nil x = False
inL (cons x xs) y = if x == y then True else (inL xs y)

___________________________
d. Especificar una función que elimina todas las ocurrencias de un elemento dado.

delAll : List A -> A -> List A

delAll nil x = nil
delAll (cons x xs) y = if x == y then (delAll xs y) else (cons x (delAll xs y))

-}