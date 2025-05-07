-- Tipo de datos para Red-Black Trees.
data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

-- Función que determina si un elemento pertenece a un RBT.
-- Mismo código que para BSTs: simplemente ignoramos el color.
member :: Ord a => a -> RBT a -> Bool
member a E = False
member a (T _ l b r)
    | a == b = True
    | a < b = member a l
    | a > b = member a r

-- Insertar elemento en un RBT.
insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where
        ins :: Ord a => a -> RBT a -> RBT a
        ins x E = T R E x E
        ins x (T c l y r)
            | x < y = balance c (ins x l) y r
            | x > y = balance c l y (ins x r)
            | otherwise = T c l y r -- Si el valor ya existía, omitimos el caso.
        
        makeBlack :: RBT a -> RBT a
        makeBlack E = E
        makeBlack (T _ l x r) = T B l x r

-- Rebalanceo de Red-Black Trees para arreglar posible violación del invariante 1: que un nodo rojo tenga un hijo rojo: B-R-R.
balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

-- Función que devuelve el mínimo elemento de un RBT.
minRBT :: Ord a => RBT a -> Maybe a
minRBT E = Nothing
minRBT (T _ E a r) = Just a
minRBT (T _ l a r) = minRBT l

-- Función principal para eliminar un elemento de un RBT.
delete :: Ord a => a -> RBT a -> RBT a
delete x t = makeBlack (del x t)
  where
    -- Función auxiliar para eliminar un elemento.
    del :: Ord a => a -> RBT a -> RBT a
    del _ E = E  -- Árbol vacío, no hay nada que eliminar.
    del x (T c l a r)
      | x < a     = balanceDel c (del x l) a r
      | x > a     = balanceDel c l a (del x r)
      | otherwise = case r of
          E -> l
          _ -> let (Just y) = minRBT r
               in balanceDel c l y (del y r)

    makeBlack :: RBT a -> RBT a
    makeBlack E = E
    makeBlack (T _ l x r) = T B l x r

    balanceDel :: Color -> RBT a -> a -> RBT a -> RBT a
    balanceDel B (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
    balanceDel B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    balanceDel B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    balanceDel B (T R a x b) y c = T B a x (T R b y c)
    balanceDel B a x (T R b y c) = T B (T R a x b) y c
    balanceDel c l a r = T c l a r

-- Ejemplos y casos de prueba.
rbt1 = insert 1 (insert 2 (insert 3 (insert 4 (insert 5 E))))