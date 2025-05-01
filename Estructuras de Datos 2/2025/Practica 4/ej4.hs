-- Tipo de datos para Red-Black Trees.
data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

-- Rebalanceo en rama de subárbol izquierdo.
lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

-- Rebalanceo en rama de subárbol derecho.
rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

-- Insertar elemento en un RBT.
insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where
        ins :: Ord a => a -> RBT a -> RBT a
        ins x E = T R E x E
        ins x (T c l y r)
            | x < y = lbalance c (ins x l) y r
            | x > y = rbalance c l y (ins x r)
            | otherwise = T c l y r -- Si el valor ya existía, omitimos el caso.
        
        makeBlack :: RBT a -> RBT a
        makeBlack E = E
        makeBlack (T _ l x r) = T B l x r

-- Ejemplos y casos de prueba.
rbt1 = insert 1 (insert 2 (insert 3 (insert 4 (insert 5 E))))