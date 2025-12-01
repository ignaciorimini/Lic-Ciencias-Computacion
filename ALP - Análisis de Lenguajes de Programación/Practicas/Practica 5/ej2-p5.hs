-- 2) Probar que las siguientes instancias no son correctas
-- (no cumplen las leyes de los functores).

-------------------------------
-- a)
data Func a = Func (a -> a)

instance Functor Func where
    fmap g (Func h) = Func id

-- Functor.1: fmap id = id 
-- ∀ func :: Func, fmap id func = id func
-- Sea func = Func h

-- = fmap id func
-- = fmap id (Func h)           <-- def func
-- = Func id                    <-- def fmap
-- \= id (Func h)


-------------------------------
-- b)
data Br b a = B b (a, a)

instance Functor (Br b) where
    fmap f (B x (y, z)) = B x (f z, f y)

-- Functor.1: fmap id = id
-- ∀ br :: Br, fmap id br = id br
-- Sea br = B x (y, z)

-- = fmap id br
-- = fmap id (B x (y, z))       <-- def br
-- = B x (id z, id y)           <-- def fmap
-- = B x (z, y)                 <-- def id
-- \= id (B x (y, z))           <-- def id inversa
-- Intercambia el orden de los elementos del par.