-- Dar el tipo completo de las siguientes funciones
-- a) test donde test f x = f x == x + 2

test :: (Eq a, Num a) => (a -> a) -> a -> Bool
test f x = f x == x + 2

-- b) esMenor donde esMenor y z = y < z
esMenor :: Ord a => a -> a -> Bool
esMenor y z = y < z

-- c) eq donde eq a b = a == b
eq :: Eq a => a -> a -> Bool
eq a b = a == b

-- d) showVal donde showVal x = "Valor:" ++ show x
showVal :: Show a => a -> String
showVal x = "Valor: " ++ show x