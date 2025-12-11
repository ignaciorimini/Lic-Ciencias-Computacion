-- 6) Se desea modelar computaciones con un estado global s.
-- Para esto se define el siguiente tipo de datos e instancia de mónada

-- St :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))
newtype State s a = St { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f (St h) = St (\s -> 
        let (x, s') = h s
        in (f x, s'))

instance Applicative (State s) where
    pure = return

instance Monad (State s) where
    -- return :: a -> State s a
    return x = St (\s -> (x, s))

    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    -- h :: s -> (a, s)
    -- runState (f x) :: s -> (b, s)
    (St h) >>= f = St (\s -> let (x, s') = h s 
                             in runState (f x) s')

-------------------------------------
-- a) Probar que la instancia efectivamente define una mónada.

-- Monad.1: return a >>= k = k a
-- = return a >>= k
-- = St (\s -> (a, s)) >>= k                    -- def return
-- = St (\s -> let (x, s') = (\t -> (a, t)) s   -- def >>=
--             in runState (k x) s')
-- = St (\s -> runState (k a) s)                -- x = a, s' = s por b-red
-- = k a
-- La definición de una acción de estado m es St (runState m).
-- Por lo tanto St (\s -> runState (k a) s) es exactamente k a.

-- Monad.2: m >>= return = m
-- = State (\s -> (a, s)) >>= return
-- = St (\s -> let (x,s') = (\t -> (a, t)) s    -- def >>=
--             in runState (return x) s')
-- = St (\s -> runState (return a) s)           -- x = a, s' = s, b-red
-- = St (\s -> runState (St (\t -> (a, t))) s)  -- def return
-- = St (\s -> (\t -> (a, t)) s)                -- def runState
-- = St (\s -> (a, s))                          -- b-red
-- = m

-- Monad.3: m >>= (\x -> k h >>= h) = (m >>= k) >>= h
-- = State (\s -> (a, s)) >>= (\x -> k x >>= h)
-- = ... (COMPLETAR)

-------------------------------------
-- b) Definir operaciones set :: s -> State s () y get :: State s s
-- que permiten actualizar el estado y leerlo, respectivamente.

set :: s -> State s ()
set nuevoEstado = St (\sAntiguo -> ((), nuevoEstado))

get :: State s s
get = St (\sActual -> (sActual, sActual))

-- modify :: (s -> s) -> State s ()
-- Función auxiliar que usa get y set para cambiar el estado
modify :: (s -> s) -> State s ()
modify f = do
    s <- get         -- Obtener el estado actual (s)
    set (f s)        -- Aplicar la función f y establecer el nuevo estado (f s)

-------------------------------------
-- c. Funciones de Prueba

-- Una computación simple que devuelve el valor del estado,
-- pero que duplica el estado antes de devolverlo.
ejemplo_get_set :: State Int Int
ejemplo_get_set = do
    -- Estado inicial: s
    valor_original <- get  -- x = s. Estado sigue siendo s.
    modify (*2)            -- El estado ahora es s * 2.
    return valor_original  -- El valor de retorno es s.
-- runState ejemplo_get_set 5 -> (5,10)

-- Una computación que simula un contador de pasos y muestra cómo se encadena el estado.
contador :: State Int String
contador = do
    -- Estado inicial s = 0 (cuando lo ejecutemos)
    modify (+1)            -- Estado: 1
    modify (+1)            -- Estado: 2
    paso2 <- get           -- paso2 = 2. Estado: 2
    modify (\n -> n * 10)  -- Estado: 20
    paso20 <- get          -- paso20 = 20. Estado: 20
    return $ "Valor obtenido en el paso 2: " ++ show paso2 ++ ". Estado final: " ++ show paso20
-- runState contador 0 -> ("Valor obtenido en el paso 2: 2. Estado final: 20",20)