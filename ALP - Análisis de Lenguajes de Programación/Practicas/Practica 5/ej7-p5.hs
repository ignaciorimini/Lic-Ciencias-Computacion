-- Observacion. Dado que toda instancia de Monad debe tener una instancia
-- en la clase Applicative, y que todo Applicative debe tener una instancia
-- en Functor, podemos dar las siguientes instancias para cualquier mónada m
-- y ocuparnos solo de la instancia de Monad:

instance Functor M where
    fmap = liftM

instance Applicative M where
    pure = return
    (<*>) = ap

-- donde las operaciones ap y liftM se exportan de los siguientes módulos.
-- import Control.Applicative (Applicative (..))
-- import Contorl.Monad (liftM, ap)

-- liftM implementa fmap usando las operaciones de las mónadas.
liftM :: Monad m => (a -> b) -> m a -> m b
liftM g m = m >>= (\x -> return (g x))

-- ap implementa <*> usando las operaciones de las mónadas.
ap :: Monad m => m (a -> b) -> m a -> m b
ap f x = f >>= (\g -> x >>= (y -> return (g y)))

---------------------------------------------------
-- 7) Dados los siguientes tipos de datos:
newtype Id a = Id a
data Either e a = Left e | Right a

-- Dar la instancia de Monad para Id y demostrar que valen las leyes de las mónadas.
instance Monad Id where
    -- return :: a -> m a
    return x = Id x

    -- (>>=) :: m a -> (a -> m b) -> m b
    (Id x) >>= f = (f x)

-- Monad.1: return a >>= k = k a
-- = return a >>= k
-- = (Id a) >>= k                   <-- def Id return
-- = k a                            <-- def Id >>=

-- Monad.2: m >>= return = m
-- = (Id x) >>= return
-- = return x                       <-- def Id >>=
-- = Id x                           <-- def Id return

-- Monad.3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- = (Id x >>= k) >>= h
-- = k x >>= h                      <-- def Id >>=
-- = h (k x)                        <-- def Id >>=

-- = (Id x) >>= (\y -> k y >>= h)
-- = (\y -> k y >>= h)  x           <-- def Id >>=
-- = k x >>= h                      <-- b-red
-- = h (k x)                        <-- def Id >>=


---------------------------------------------------
-- Dar la instancia de Monad para Either y demostrar que valen las leyes de las mónadas.
instance Monad Either where
    -- return :: a -> m a
    return x = Right x

    -- (>>=) :: m a -> (a -> m b) -> m b
    (Left err) >>= f = Left err 
    (Right x) >> f = f x

-- Demostración de leyes monádicas usando inducción estructural
-- sobre el tipo de datos Either e a = Left e | Right a

-----------
-- Monad.1: return a >>= k = k a
-- = return a >>= k
-- = (Right a) >>= k                <-- def Either return
-- = k a                            <-- def Either.2 >>=

-----------
-- Monad.2: m >>= return = m

-- Caso m = (Left x)
-- = m >>= return
-- = (Left x) >>= return                <-- def m
-- = Left x                             <-- def Either.1 >>

-- Caso m = (Right x)
-- = m >>= return
-- = (Right x) >>= return               <-- def m
-- = return x                           <-- def Either.2 >>=
-- = Right x                            <-- def Either return

-----------
-- Monad.3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h

-- Caso m = (Left err)
-- = (m >>= k) >>= h
-- = ((Left err) >>= k) >>= h           <-- def m
-- = Left err >>= h                     <-- def Either.1 >>=    
-- = Left err                           <-- def Either.1 >>=

-- = m >>= (\x -> k x >>= h)
-- = (Left err) >>= (\x -> k x >>= h)   <-- def m
-- = Left err                           <-- def Either.1 >>=

-- Caso m = (Right x)
-- = (m >>= k) >>= h
-- = ((Right x) >>= k) >>= h            <-- def m
-- = k x >>= h                          <-- def Either.2 >>=

-- = m >>= (\x -> k x >>= h)
-- = (Right x) >>= (\x -> k x >>= h)    <-- def m
-- = (\x -> k x >>= h) x                <-- def Either.2 >>=
-- = k x >>= h                          <-- b-red
