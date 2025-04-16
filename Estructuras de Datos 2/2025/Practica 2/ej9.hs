-- Función zip3 de forma recursiva.
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs
zip3' _ _ _ = []

-- Función zip3 usando zip.
zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'' xs ys zs = [(x,y,z) | ((x,y),z) <- zip (zip xs ys) zs]