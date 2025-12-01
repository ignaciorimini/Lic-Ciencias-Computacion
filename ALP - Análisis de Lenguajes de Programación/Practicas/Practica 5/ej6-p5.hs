-- 6) Probar que toda mónada es un functor, es decir, proveer una instancia:

-- La clase Monad en Haskell tiene la siguiente definición:
class Applicativ m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

-- Asumimos que m es una mónada para definir fmap.
-- Tomamos el valor enuelto ma :: m a.
-- Usamos >>= para desenvolver el valor a de su contexto m.
-- Aplicamos la función pura f :: a -> b, obteniendo f a :: b.
-- Usamos return para envolver el resultado f a en el contexto m.
instance Monad m => Functor m where
    -- fmap :: (a -> b) -> m a -> m b
    fmap f ma = ma >>= (\a -> return (f a))

-- Functor.1: fmap id m = id m
-- = fmap id m
-- = m >>= (\a -> return (id a))        <-- def fmap
-- = m >>= (\a -> return a)             <-- def id
-- = m >>= return                       <-- trivial
-- = m                                  <-- ley Monad.2

-- Functor.2: (fmap f . fmap g) m = fmap (f . g) m

-- = (fmap f . fmap g) m
-- = fmap f (fmap g m)                                      <-- def (.)
-- = fmap f (m >>= (\a -> return (g a)))                    <-- def fmap
-- = (m >>= (\a -> return (g a)))) >>= (\b -> return (f b)) <-- def fmap
-- = m >>= (\x -> k x >>= h )                               <-- ley Monad.3 con m' = m, k = \a -> return (g a), h = \b -> return (f b)
-- = m >>= (\x -> (\a -> return (g a)) x >>= h)             <-- def k
-- = m >>= (\x -> return (g x) >>= h)                       <-- b-red
-- = m >>= (\x -> h (g x))                                  <-- ley Monad.1 con a = (g x), k = h
-- = m >>= (\x -> (\b -> return (f b)) (g x))               <-- def h
-- = m >>= (\x -> return (f (g x)))                         <-- b-red

-- = fmap (f . g) m
-- = m >>= (\a -> return ((f . g) a))       <-- def fmap
-- = m >> (\x -> return (f (g x)))          <-- def (.) y alpha-conv

-- Como llegamos a que ambas partes de la igualdad son iguales, vale la propiedad Functor.2