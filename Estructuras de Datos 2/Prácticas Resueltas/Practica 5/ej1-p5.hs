{-
1) Operaciones
tad List A where
    import Bool
    nil : List A
    cons : A -> List A -> List A
    null : List A -> Bool
    head : List A -> A
    tail : List A -> List A

___________________________________________________
a) Dar una especificación algebraica del TAD listas finitas.

null nil = True
null (cons x nil) = False
head (cons x xs) = x
tail (cons x xs) = xs

___________________________________________________
b) Dar una especificación tomando como modelo las secuencias.

Tomamos como modelo las secuencias <x1, x2, ... xn> y para cada operación damos una función equivalente sobre este modelo.

nil = <>
cons x nil = <x>
cons x <x1, x2, ... xn> = <x, x1, ... xn>
head <x1, x2, ... xn> = x1
tail <x1, x2, ... xn> = <x2, ... xn>
null <x1, x2, ... xn> = True si n = 0
null <x1, x2, ... xn> = False en otro caso

___________________________________________________
c) Asumiendo que A es un tipo con igualdad, especificar una función inL : List A -> A -> Bool tal que:
inL ls x = True si y sólo si x es un elemento de ls.

inL nil x = False
inL (cons x xs) y = if (x == y) then true else (inL xs y)

___________________________________________________
d) Especificar una función que elimina todas las ocurrencias de un elemento dado.

elim x nil = nil
elim x (cons y ys) = if (x == y) then (elim x ys) else (cons y (elim x ys))

-}

