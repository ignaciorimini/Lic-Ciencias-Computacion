data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

-- Toma una CList y devuelve el primer elemento de ella.
headCL :: CList a -> a
headCL (CUnit v) = v
headCL (Consnoc v1 xs v2) = v1

-- Toma una CList y devuelve la misma sin el primer elemento.
tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ EmptyCL v2) = CUnit v2
tailCL (Consnoc _ (CUnit v2) v3) = Consnoc v2 EmptyCL v3
tailCL (Consnoc _ xs v2) = Consnoc (headCL xs) (tailCL xs) v2

-- Devuelve True si una CList tiene un solo elemento, o False en caso contrario.
isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

-- Toma una CList y devuelve su inversa.
reverseCL :: CList a -> CList a
reverseCL (Consnoc v1 xs v2) = Consnoc v2 (reverseCL xs) v1
reverseCL l = l

-- Toma una CList y devuelve una CList con todos los posibles inicios de la CList.
inits :: CList a -> CList (CList a)
inits EmptyCL = EmptyCL
inits (CUnit v) = CUnit (CUnit v)
inits (Consnoc v1 xs v2) = 

-- Toma una CList de CList y devuelve la CList con todas ellas concatenadas.
-- concatCL :: CList (CList a) -> CList a
-- concatCL EmptyCL = EmptyCL
-- concatCL (CUnit l) = concatCL l
-- concatCL (Consnoc l1 l2 l3) = l1 

-- ----------------------------------------------------
-- Ejemplos de CList Integer para probar las funciones.
listaEjemplo = Consnoc 3 (Consnoc 5 (CUnit 3) 1) 4
listaEjemplo2 = Consnoc 3 (CUnit 2) 4
listaEjemplo3 = Consnoc 3 EmptyCL 4
listaEjemplo4 = Consnoc 3 (Consnoc 4 (Consnoc 2 EmptyCL 3) 1) 3
listaEjemplo5 = EmptyCL
listaEjemplo6 = CUnit 3
listaEjemplo7 = CUnit EmptyCL
listaEjemplo8 = CUnit (CUnit 3)
listaEjemplo9 = Consnoc (CUnit 1) (CUnit (CUnit 2)) (CUnit 3)