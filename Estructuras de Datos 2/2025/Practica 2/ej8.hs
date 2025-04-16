-- Determinar el tipo de h.
f :: c -> d
g :: a -> b -> c

h :: a -> b -> d
h x y = f (g x y)

-- Definiciones de h equivalentes a la dada.
h x = f . (g x)
h x y = (f . g) x y

-- Dar el tipo de la función .
. :: (b -> c) -> (a -> b) -> a -> c