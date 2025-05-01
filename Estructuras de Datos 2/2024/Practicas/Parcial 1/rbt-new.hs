data Color = R | B deriving (Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show)

member :: Ord a => a -> RBT a -> Bool
member _ E = False
member x (T _ l v r)
    | x == v = True
    | x < v = member x l
    | otherwise = member x r


insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where 
        ins x E = T R E x E
        ins x t@(T c l y r)
            | x == y = t
            | x < y = balance (T c (ins x l) y r)
            | otherwise = balance (T c l y (ins x r))
        
        makeBlack E = E
        makeBlack (T _ l v r) = T B l v r


balance :: RBT a -> RBT a
balance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
balance (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
balance t = t


toTree :: Ord a => [a] -> RBT a
toTree [] = E
toTree (x:xs) = insert x (toTree xs)