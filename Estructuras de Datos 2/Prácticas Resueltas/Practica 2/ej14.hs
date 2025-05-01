-- Producto escalar de dos listas de enteros de igual longitud: suma de los productos delos elementos sucesivos (misma posición) de ambas listas.
-- scalarProduct [1, 2, 3] [4, 5, 6] -> (1*4 + 2*5 + 3*6) = 4 + 10 + 18 = 32

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [a*b | (a,b) <- zip xs ys]