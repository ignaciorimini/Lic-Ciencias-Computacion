-- Pasar de notación Haskell a notación de funciones anónimas (notación lambda).

-- a)
smallest :: Ord a => (a,a,a) -> a
smallest (x,y,z)| x <= y && x <= z = x 
                | y <= x && y <= z = y
                | z <= x && z <= y = z

smallest' :: Ord a => (a,a,a) -> a
smallest' = \(x,y,z) -> min (min x y) z

-- b)
second :: p1 -> p2 -> p2
second x = \x -> x

second' :: p1 -> p2 -> p2
second' = \_ y -> y

-- c)
andThen :: Bool -> Bool -> Bool
andThen True y = y
andThen False y = False

andThen' :: Bool -> Bool -> Bool
andThen' = \x y -> if x then y else False

-- d)
twice :: (t -> t) -> t -> t
twice f x = f (f x)

twice' :: (t -> t) -> t -> t
twice' = \f x -> f (f x)

-- e)
flip2 :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip2 f x y = f y x

flip2' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip2' = \f x y -> f y x

-- f)
inc :: Integer -> Integer
inc = (+1)

inc' :: Integer -> Integer
inc' = \x -> x + 1