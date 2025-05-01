data Linea = Linea [Char] Int deriving Show

vacia :: Linea
vacia = Linea [] 0

moverIzq :: Linea -> Linea
moverIzq l@(Linea _ 0) = l
moverIzq (Linea xs c) = (Linea xs (c-1))

moverDer :: Linea -> Linea
moverDer l@(Linea xs c) | c == length xs = l
                        | otherwise = (Linea xs (c+1))

moverIni :: Linea -> Linea
moverIni (Linea xs c) = (Linea xs 0)

moverFin :: Linea -> Linea
moverFin (Linea xs _) = (Linea xs (length xs))

borrar :: Linea -> Linea
borrar l@(Linea xs 0) = l
borrar (Linea xs c) = 
    let nuevaLista = (take (c-1) xs) ++ (drop c xs)
    in (Linea nuevaLista (c-1))

insertar :: Char -> Linea -> Linea
insertar c (Linea xs p) = 
    let nuevaLista = (take p xs) ++ [c] ++ (drop p xs)
    in (Linea nuevaLista (p+1))
    
-- linea1 = Linea "hola" 4
-- insertar 'A' linea1
-- Linea "holaA" 5

