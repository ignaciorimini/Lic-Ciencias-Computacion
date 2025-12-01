-- 3) Dar la instancia de Applicative para:

-- Observación: 
-- La sintaxis class Functor (Either e) => Applicative (Either e) where,
-- solo se usa cuando se define una nueva clase de tipos, no cuando se
-- crea una instancia para un tipo de datos específico.

---------------------------
-- a) Either e, con e fijo.
class Applicative (Either e) where
    -- pure :: a -> Either e a
    pure x = Right x

    -- (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Left err) <*> _ = Left err
    (Right f) (<*>) (Left err) = Left err
    (Right f) (<*>) (Right x) = Right (f x)


---------------------------
-- b) (->) r, con r fijo.
-- El tipo (->) r a es el tipo de una función que toma un valor de tipo r
-- y produce un valor de tipo a. Al fijar el tipo r, estamos modelando
-- una computación que necesita un entorno r para ejecutarse. 
-- Esta es la Mónada Reader vista en clase.
class Functor ((->) r) => Applicative ((->) r) where
    -- pure :: a -> (r -> a)
    -- Debe devolver una función de tipo r -> a que siempre produce x. 
    -- Simplemente creamos una función que ignora su entrada (r).
    pure x = \_ -> x 

    -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
    -- Tenemos dos funciones: f_r :: r -> (a -> b) y x_r :: r -> a.
    -- Queremos combinarlas para obtener una nueva función de tipo r -> b.
    -- La nueva función toma el entorno r.
    -- Al darle r a f_r, obtenemos la función pura f :: a -> b
    -- Al darle r a x_r, obtenemos el valor puro x :: a.
    -- Al aplicar f x, tenemos (f_r r) (x_r r) :: r -> b.
    (<*>) f_r x_r = \r -> (f_r r) (x_r r)