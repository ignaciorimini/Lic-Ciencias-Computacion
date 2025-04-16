-- Dar el tipo completo de las siguientes funciones.

test :: (Eq a, Num a) => (a -> a) -> a -> Bool
test f x = f x == x + 2

esMenor :: Ord a => a -> a -> Bool
esMenor y z = y < z

eq :: Eq a => a -> a -> Bool
eq a b = a == b

showVal :: Show a => a -> String
showVal x = "Valor:" ++ show x