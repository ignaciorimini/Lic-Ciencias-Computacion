module Test where
-- Reescribir cada una de las siguientes definiciones sin usar let, where o if:

-------------------------------
-- a) f x = let (y, z) = (x, x) in y
f1 :: Int -> Int
f1 x = 
    let (y, z) = (x, x)
    in y

f1' :: Int -> Int
f1' x = x


-------------------------------
-- b) greater (x, y) = if x > y then True else False
greater :: Ord a => (a, a) -> Bool
greater a
    | fst a > snd a = True
    | otherwise     = False


-------------------------------
-- c) f (x, y) = let z = x + y in g (z, y) where g (a, b) = a - b
f2 :: Num a => a -> a -> a
f2 x y = 
    let z = x + y
    in g (z, y)
    where g (a, b) = a - b

f2' :: Num a => (a, a) -> a
f2' par = fst par + snd par - snd par