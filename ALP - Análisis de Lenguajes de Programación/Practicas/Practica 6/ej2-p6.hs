-- 2) Dar una definición de la función getChars :: Int -> IO String,
-- que dado n lea n caracteres del teclado, usando las funciones
-- sequenceA y replicate.

-- replicate n getChar
-- Crea una lista de n copias de la accion monádica getChar. 
-- El tipo devuelto es [IO Char] (lista de acciones).

-- sequenceA :: Applicative f => [f a] -> f [a]
-- Toma una lista de acciones ([IO Char]) y las ejecuta secuencialmente.
-- Recopila los resultados puros (los Char) en una lista,
-- y envuelve la lista resultante en una acción IO.
-- El tipo devuelto es IO [Char] o IO String.

import System.IO (getChar)

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (f:fs) = pure (:) <*> f <*> sequenceA' fs

getChars :: Int -> IO String
getChars n =
    let listOfActions = replicate n getChar
    in sequenceA' listOfActions

-- Ejecutar ghci ej2-p6.hs
-- getChars 1
-- > a -> "a"
-- getChars 2
-- > ab -> "ab"
-- getChars 5
-- > hola! -> "hola!"