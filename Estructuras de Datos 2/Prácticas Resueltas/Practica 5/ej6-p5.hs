{-
6) Demostrar que (uncurry zip) . unzip = id

zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y):ps) = (x:xs, y:ys)
    where
        (xs,ys) = unzip ps

-- La función uncurry transforma una función currificada que toma dos argumentos secuenciales en una función que toma un único par (tupla) como argumento.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

___________________________________________________
Queremos demostrar la siguiente propiedad empleando inducción fuerte sobre la longitud de la lista:

Q(n): length xs = n => ((uncurry zip) . unzip) xs = xs (∀xs :: [a])

- Caso base: length xs = 0 -> xs = []
= ((uncurry zip) . unzip) []
= (uncurry zip) (unzip [])      <. def (composición)>
= (uncurry zip) ([], [])        <unzip def 1>
= zip [] []                     <uncurry def>
= []                            <zip def 1>

Y por lo tanto, se cumple el caso base: xs = [].

- Caso inductivo: length xs = n. 
Supongamos que se cumple el enunciado para toda lista xs de longitud k < n (HI) y probemos para una lista de longitud n: (x,y):ps
= ((uncurry zip) . unzip) (x,y):ps
= (uncurry zip) (unzip (x,y):ps)        <. def (composicion)>
= (uncurry zip) (x:xs, y:ys)            <unzip def 2>
= zip (x:xs) (y:ys)                     <uncurry def>
= (x,y) : (zip xs ys)                   <zip def 3>
= (x,y) : ((uncurry zip) (xs, ys))      <uncurry def>
= (x,y) : ((uncurry zip) (unzip ps))    <unzip def 2>
= (x,y) : ps                            <HI length ps < n>

Por lo tanto, se cumple el caso inductivo. Luego, hemos probado la propiedad utilizando el principio de inducción fuerte de naturales sobre la longitud de la lista.
-}