{-
Demostrar que (uncurry zip) . unzip = id, siendo:

zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

unzip :: [(a, b)] -> ([a], [b])
unzip []  = ([], [])
unzip ((x:y):ps) = (x:xs, y:ys) where (xs, ys) = unzip ps

uncurryZip :: ([a], [b]) -> [(a, b)]
uncurryZip = uncurry zip

______________________________________________
Probaremos la siguiente propiedad utilizando inducción fuerte sobre la longitud de la lista.

Q(n): length xs = n => ((uncurry zip) . unzip) xs = xs (∀xs :: [a])

Definición. Para probar Q(n) para todo n Natural, probamos que para cualquier m, si vale Q(i) ∀i < m, entonces vale Q(m).

Suponemos (HI) que ∀i < n, si length xs = i, entonces ((uncurry zip) . unzip) xs = xs.

Hacemos un análisis por casos de n:
Si n = 0, entonces tenemos que length xs = 0, es decir, xs = []:
    = ((uncurry zip) . unzip) []
    = (uncurry zip) (unzip [])          <unzip 1era def>
    = (uncurry zip) ([], [])            <uncurry def>
    = zip [] []                         <zip 1era def>
    = []

Si n > 0, suponemos Q(ps) vale y probamos Q(x:y:ps)
    = ((uncurry zip) . unzip) (x:y:ps)
    = (uncurry zip) (unzip (x:y:ps))                            <unzip 2da def>
    = (uncurry zip) (x:xs, y:ys) where (xs, ys) = unzip ps      <uncurry def>
    = zip (x:xs) (y:ys) where (xs, ys) = unzip ps               <zip 3era def>
    = (x, y) : zip xs ys where (xs, ys) = unzip ps              <uncurry def>
    = (x, y) : (uncurry zip) (xs, ys) where (xs, ys) = unzip ps 
    = (x, y) : (uncurry zip) (unzip ps)                         <HI>
    = (x, y) : ps
    = (x:y:ps)


Y así, hemos probado que vale length xs = n => ((uncurry zip) . unzip) xs = xs (∀xs :: [a]).
-}