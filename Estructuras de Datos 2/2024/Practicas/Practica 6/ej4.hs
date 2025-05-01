import GHC.Conc (par, pseq)

data T a = E | N (T a) a (T a) deriving Show

altura :: T a -> Int
altura E = 0
altura (N l x r) = 1 + (max (altura l) (altura r))

combinar :: T a -> T a -> T a
combinar E t = t
combinar (N l v r) t2 = N (combinar l r) v t2

filterT :: (a -> Bool) -> T a -> T a
filterT p E = E
filterT p (N izq val der) = 
    let ((izq', der'), val') = ((filterT p izq, filterT p der), p val)
    in if val'
        then (N izq' val der')
        else combinar izq' der'

quicksortT :: T Int -> T Int
quicksortT E = E
quicksortT (N izq val der) = 
    let (l', r') = filterT (<= val) izq || filterT (<= val) der
        (l'', r'') = filterT (> val) izq || filterT (> val) der
        (menores, mayores) = quicksortT (combinar l' r') || quicksortT (combinar l'' r'')
    in (N menores val mayores)




-- EXPLICACIÓN DE LOS COSTOS
-- SfilterT(d)
-- Realizamos <filterT p izq> y <filterT p der> en cada altura del árbol, esto sumaría Sfilter = max(Sfilter(d1), Sfilter(d2)) donde d1 y d2 son las alturas de los subárboles izq y der, esto es, Sfilter = O(d) pues el máximo entre d1 y d2 es la altura del árbol.
-- Luego, por cada nivel del árbol, en el peor caso posible, ejecutaríamos <combinar> que es del orden O(d1). Por lo tanto, Sfilter(d) = O(d^2).