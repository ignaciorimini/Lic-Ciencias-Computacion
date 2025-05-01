-- Pasar de notación lambda a notación Haskell.

-- a)
iff :: Bool -> Bool -> Bool
iff = \x -> \y -> if x then not y else y

iff' :: Bool -> Bool -> Bool
iff' x y = if x then not y else y

-- b)
alpha :: a -> a
alpha = \x -> x

alpha' :: a -> a
alpha' x = x