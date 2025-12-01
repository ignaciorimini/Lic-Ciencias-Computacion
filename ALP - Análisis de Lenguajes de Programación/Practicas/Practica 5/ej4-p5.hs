-- 4) Las funciones liftAx aplican una función a x argumentos contenidos
-- en una estructura. Usando los operadores de la clase Applicative,
-- dar las definiciones de:

-- Observación:
-- La clave es usar pure para elevar la función g al contexto y luego usar
-- <*> para aplicarla secuencialmente a los argumentos envueltos.

----------------------------
-- a) liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g va vb = pure g <*> va <*> vb

-- g :: a -> b -> c
-- pure g :: f (a -> b -> c)
-- pure g <*> va, aplica primero el argumento envuelto va :: f a,
-- por lo tanto el resultado es pure g <*> va :: f (b -> c).
-- (...) <*> vb, aplica la función al argumento envuelto vb :: f b,
-- y luego el resultado final es :: f c

-- Ejemplo en consola.
-- liftA2 (,) (Just 3) (Just 5) -> Just (3,5)
-- liftA2 (+) (Just 3) (Just 5) -> Just 8

----------------------------
-- b) liftA5
liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k) -> f a -> f b -> f c -> f d -> f e -> f k
liftA5 g va vb vc vd ve = pure g <*> va <*> vb <*> vc <*> vd <*> ve

-- El patrón para cualquier liftAx es:
-- liftAx g v1 v2 ... vx = pure g <*> v1 <*> v2 <*> ... <*> vx