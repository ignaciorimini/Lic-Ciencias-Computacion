-- f :: c -> d
-- g :: a -> b -> c
-- h x y = f (g x y)


-- Determinar el tipo de h.
-- Tipo de h.
-- h :: a -> b -> d


-- Indicar cuál de las siguientes definiciones de h son equivalentes a la dada:

-- h = f . g
-- Esta función compone f con g. Luego, h recibiría 2 argumentos x y correspondientes a la entrada de g. Luego esta función es igual que la definición de h dada.

-- h x = f . (g x)
-- Esta función aplica parcialmente g a x antes de componerla con f. Dado que x tiene tipo a, esta parcial aplicación resultará en una función de tipo b -> d. Entonces el tipo de h sería: h :: a -> (b -> d) lo cual es diferente al tipo de h.

-- h x y = (f . g) x y
-- Esta función recibe dos argumentos x y mientras compone f con g y le aplica tal composición a x y. Esto seria f(g(x y)) = f (g x y) que es igual a la definición dada de h.


-- Dar el tipo de la función composición (.)
-- . :: (b -> c) -> (a -> b) -> (a -> c)
