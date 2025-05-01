module Test where

-- La función zip3 zipea 3 listas. Dar una definición recursiva de la función y otra definición con el mismo tipo que utilice la función zip. ¿Qué ventajas y desventajas tiene cada definición?

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- La función zipWith en Haskell toma una función y dos listas como argumentos, y devuelve una nueva lista que es el resultado de aplicar la función a los elementos correspondientes de las dos listas.

zip3'' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3'' xs ys zs = zipWith (\(x, y) z -> (x, y, z)) (zip xs ys) zs
