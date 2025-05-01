{-
2) Operaciones TAD de pila
tad Stack (A:Set) where
    empty : Stack A
    push : A -> Stack A -> Stack A
    isEmpty : Stack A -> Bool
    top : Stack A -> A
    pop : Stack A -> Stack A

Especificación algebraica del TAD pilas tomando como modelo las secuencias <x1, x2, ... xn>

empty = <>
push x <> = <x>
push x <x1, x2, ... xn> = <x, x1, x2, ... xn>
isEmpty <x1, x2, ... xn> = True si n = 0
isEmpty <x1, x2, ... xn> = False en otro caso
top <x1, x2, ... xn> = x1
pop <x1, x2, ... xn> = <x2, ... xn>
pop <> = <>

-}