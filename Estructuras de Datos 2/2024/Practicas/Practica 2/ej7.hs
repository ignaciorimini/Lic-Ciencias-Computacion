module Test where

-- Pasar de notación lambda a notación Haskell.

-- a) iff = \x -> \y -> if x then not y else y
-- Función idéntica a un XOR (OR exclusivo).
iff :: Bool -> Bool -> Bool
iff x y = 
    if x
        then not y
        else y


-- b) alpha = \x -> x
alpha :: a -> a
alpha x = x