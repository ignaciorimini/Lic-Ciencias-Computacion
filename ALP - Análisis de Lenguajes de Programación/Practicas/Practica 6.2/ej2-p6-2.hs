-- 2) Dar una definición de la función getChars :: Int -> IO String,
-- que dado n lea n caracteres del teclado, usando las funciones
-- sequenceA y replicate.

-- replicate :: Int -> a -> [a]
-- Crea una lista que contiene N copias del mismo elemento.
-- replicate 3 'c' -> ['c', 'c', 'c']
-- replicate 0 5 -> []
-- replicate n getChar -> [getChar :: IO Char, getChar :: IO Char, ...]

-- sequenceA :: Applicative f => [f a] -> f [a]
-- Toma una lista de acciones envueltas y las ejecuta secuencialmente.
-- Recopila los resultados puros en una lista, y envuelve la lista 
-- resultante.
-- sequenceA [Just 1, Just 2, Just 3] -> Just [1,2,3]
-- sequenceA [Just 1, Nothing, Just 3] -> Nothing

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar) 

-- > getChars 5
-- > abcde -> "abcde"
-- > getChars 2
-- > ab -> "ab