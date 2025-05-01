module Test where
-- Pasar de notación Haskell a notación de funciones anónimas (notación lambda).

-----------------------------------------------
-- a) smallest (x, y, z)| x <= y ∧ x <= z   = x
--                      | y <= x ∧ y <= z   = y
--                      | z <= x ∧ z <= y   = z

-- Observación: no se pueden usar guardias en notación lambda.

smallest :: Ord a => a -> a -> a -> a
smallest = \x y z ->
    if x <= y && x <= z
        then x
        else if y <= x && y <= z
            then y
            else z


-----------------------------------------------
-- b) second x = \lambda x -> x

second :: a -> a
second = \x -> x


-----------------------------------------------
-- c)   andThen True y = y
--      andThen False y = False
-- Es una función idéntica a un AND lógico.

andThen :: Bool -> Bool -> Bool
andThen = \x y ->
    if x
        then y
        else x


-----------------------------------------------
-- d) twice f x = f (f x)

twice = \f x -> f (f x)
-- twice (*3) 2 = (*3) ((*3) 2) = (*3) 6 -> 18


-----------------------------------------------
-- e) flip f x y = f y x

flip' = \f x y -> f y x
-- flip' (div) 5 10 -> 2
-- flip' (div) 10 5 -> 0


-----------------------------------------------
-- f) inc = (+1)

inc = \x -> x + 1
-- inc 3 = 4