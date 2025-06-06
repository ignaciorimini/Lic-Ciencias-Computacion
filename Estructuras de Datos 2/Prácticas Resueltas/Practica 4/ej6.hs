type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Rank
rank E = 0
rank (N r _ _ _) = r

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
  if x <= y then makeH x a1 (merge b1 h2)
            else makeH y a2 (merge h1 b2)
  where
    makeH x a b = if rank a >= rank b
                  then N (rank b + 1) x a b
                  else N (rank a + 1) x b a

fromList :: Ord a => [a] -> Heap a
fromList [] = E
fromList (x:xs) = merge (N 1 x E E) (fromList xs)
