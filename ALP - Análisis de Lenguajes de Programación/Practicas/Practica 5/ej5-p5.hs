-- 5) Definir una función sequenceA :: Applicative f => [f a] -> f [a]
-- que dada una lista de acciones de tipo f a, siendo f un functor
-- aplicativo, transforme la lista en una acción de tipo f [a].

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (f:fs) = pure (:) <*> f <*> sequenceA' fs

-- pure (:)
-- El constructor de lista (:) :: a -> [a] -> [a] se eleva al contexto f.
-- Su nuevo tipo: f (a -> [a] -> [a])

-- <*> f
-- Aplicamos la función envuelta al primer elemento envuelto, f : f a.
-- Resultado: f ([a] -> [a])

-- <*> sequenceA' fs
-- La llamada recursiva devuelve un tipo f [a].
-- Resultado: f [a]

-- Ejemplos en consola.
-- sequenceA' [Just 1, Just 2] -> Just [1,2]
-- sequenceA' [Just 1, Nothing, Just 2] -> Nothing
-- sequenceA' [['a','b'], ['c','d']] -> ["ac","ad","bc","bd"]