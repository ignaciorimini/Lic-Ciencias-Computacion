{-
2) Dado el TAD de pilas:
tad Stack A where
    import Bool
    empty : Stack A
    push : A -> Stack A -> Stack A
    isEmpty : Stack A -> Bool
    top : Stack A -> A
    pop : Stack A -> Stack A

___________________________________________________
a) Dar una especificación algebraica del TAD de pilas.

isEmpty empty = True
isEmpty (push x xs) = False
top (push x xs) = x
pop (push x xs) = xs

___________________________________________________
b) Dar una especificación algebraica tomando como modelo las secuencias <>.

empty = <>
push x empty = <x>
push x <x1, x2, ... xn> = <x, x1, ... xn>
top <x, x1, ... xn> = x
pop <> = <>
pop <x, x1, ... xn> = <x1, ... xn>
isEmpty <x1, x2, ... xn> = True si n = 0
isEmtpy <x1, x2, ... xn> = False en otro caso


-}