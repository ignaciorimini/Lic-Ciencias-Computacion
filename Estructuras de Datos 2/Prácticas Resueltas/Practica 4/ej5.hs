-- Tipo de datos para arboles 1-2-3.
data Tree123 a = E | Node2 a (Tree123 a) (Tree123 a) | Node3 a a (Tree123 a) (Tree123 a) (Tree123 a) | Node4 a a a (Tree123 a) (Tree123 a) (Tree123 a) (Tree123 a) deriving Show

-- Tipo de datos para red-black trees.
data Color = R | B
data RBT a = Empty | Node Color a (RBT a) (RBT a)


-- Ejemplos y casos de prueba.
arbol123 = Node3 50 90 (Node2 20 (Node2 10 E E) (Node3 30 40 E E E)) (Node2 70 (Node2 60 E E) (Node2 80 E E)) (Node3 120 150 (Node3 100 110 E E E) (Node3 130 140 E E E) (Node2 160 E E))

fromRBTo123 :: Ord a => RBT a -> Tree123
fromRBTo123 Empty = E