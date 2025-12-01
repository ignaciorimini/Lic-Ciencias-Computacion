-- Demostrar que los siguientes tipos de datos son functores. 
-- Es decir, dar su instancia de la clase Functor correspondiente
-- y probar que se cumplan las leyes de los functores.

------------------------------------------
-- a) Pares ordenados.
data Pair a = P (a, a) deriving Show

instance Functor Pair where
    fmap f (P (x,y)) = P (f x, f y)

-- Functor.1: ∀ p :: Pair a, fmap id p = id p
-- = fmap id (P (x,y))
-- = P (id x, id y)         <-- def fmap
-- = P(x, y)                <-- def id
-- = id (P (x,y))           <-- def id inversa

-- Functor.2: ∀ p :: Pair a, (fmap f . fmap g) p = fmap (f . g) p
-- = (fmap f . fmap g) (P (x,y))
-- = fmap f (fmap g (P (x,y)))          <-- def (.)
-- = fmap f (P (g x, g y))              <-- def fmap
-- = P (f g x, f g y)                   <-- def fmap
-- = P ((f . g) x, (f . g) y)           <-- def (.)
-- = fmap (f . g) (P (x, y))            <-- def fmap inversa


------------------------------------------
-- b) Árboles binarios.
data Tree a = Empty | Branch a (Tree a) (Tree a)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)

------------
-- Functor.1 ∀ t :: Tree a, fmap id t = id t
-- Demostración mediante inducción estructural sobre t :: Tree a.

-- Caso Empty.
-- = fmap id Empty
-- = Empty              <-- def fmap.1
-- = id Empty           <-- def id inversa

-- Caso Branch. Suponemos por HI que vale el enunciado para l y r
-- y probaremos para Branch x l r.
-- = fmap id (Branch x l r)
-- = Branch (id x) (fmap id l) (fmap id r)      <-- def fmap.2
-- = Branch x (fmap id l) (fmap id r)           <-- def id
-- = Branch x (id l) (id r)                     <-- HI
-- = Branch x l r                               <-- def id
-- = id (Branch x l r)                          <-- def id inversa

------------
-- Functor.2 ∀ t :: Tree a, (fmap f . fmap g) t = fmap (f . g) t
-- Demostración mediante inducción estructural sobre t :: Tree a.

-- Caso Empty.
-- (fmap f . fmap g) Empty
-- fmap f (fmap g Empty)                        <-- def (.)
-- fmap f Empty                                 <-- def fmap.2
-- Empty                                        <-- def fmap.2
-- fmap (f . g) Empty                           <-- def fmap.2 inversa

-- Caso Branch. Suponemos por HI que vale el enunciado para l y r
-- y probaremos para Branch x l r.
-- = (fmap f . fmap g) (Branch x l r)
-- = fmap f (fmap g (Branch x l r))                                 <-- def (.)
-- = fmap f (Branch (g x) (fmap g l) (fmap g r))                    <-- def fmap.2
-- = Branch (f (g x)) (fmap f (fmap g l)) (fmap f (fmap g r))       <-- def fmap.2
-- = Branch ((f . g) x) ((fmap f . fmap g) l) ((fmap f . fmap g) r) <-- def (.)
-- = Branch ((f . g) x) (fmap (f . g) l) (fmap (f . g) r)           <-- HI
-- = fmap (f . g) (Branch x l r)                                    <-- def fmap.2 inversa


------------------------------------------
-- c) Árboles genéricos.
data GenTree a = Gen a [GenTree a]

instance Functor GenTree where
    fmap f (Gen x children) = Gen (f x) (map (fmap g) children)


------------------------------------------
-- d) Continuación. Modela el contexto de una computación donde
-- el resultado a aún no se ha producido.
data Cont a = C ((a -> Int) -> Int)

instance Functor Cont where
    fmap g (C h) = C (\k -> h (k . g))

-- Tenemos h :: (a -> Int) -> Int y g :: a -> b. Queremos Cont b.
-- Cont b requiere una función de tipo (b -> Int -> Int)
-- k . g :: a -> Int ya que k :: b -> Int
-- Luego, h . (k . g) :: (a -> Int) -> Int
-- Finalmente, la funcion (\k -> h (k . g)) :: (b -> Int) -> Int
