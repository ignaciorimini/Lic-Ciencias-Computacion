module Test where

{-
Consideremos un editor de líneas simple. Supongamos que una Línea es una secuencia de caracteres c1, c2, . . . , cn
junto con una posición p, siendo 0 <= p <= n, llamada cursor (consideraremos al cursor a la derecha de un caracter
que será borrado o insertado, es decir como el cursor de la mayoría de los editores). Se requieren las siguientes operaciones sobre líneas:

vacía :: Linea
moverIzq :: Linea → Linea
moverDer :: Linea → Linea
moverIni :: Linea → Linea
moverFin :: Linea → Linea
insertar :: Char → Linea → Linea
borrar :: Linea → Linea

La descripción informal es la siguiente:
(1) la constante "vacía" denota la línea vacía.
(2) la operación "moverIzq" mueve el cursor una posición a la izquierda (siempre que ello sea posible).
(3) análogamente para "moverDer".
(4) "moverIni" mueve el cursor al comienzo de la línea.
(5) "moverFin" mueve el cursor al final de la línea.
(6) la operación "borrar" elimina el caracterer que se encuentra a la izquierda del cursor.
(7) "insertar" agrega un caracter en el lugar donde se encontraba el cursor, dejando al caracter insertado a su izquierda.

Definir un tipo de datos Linea e implementar las operaciones dadas.
-}

-- Estructura Linea:
-- Vacia no recibe argumentos y representa a la línea vacía.
-- Cadena recibe un String que representa a la secuencia de caracteres, y un Int que representa a la Posición p de donde está el cursor.
data Linea = Vacia | Cadena String Int deriving Show


vacia :: Linea 
vacia = Vacia


moverIzq :: Linea -> Linea
moverIzq Vacia = Vacia
moverIzq (Cadena s p) = 
    if p <= 0
        then (Cadena s 0)
        else
            let pNew = p - 1
            in Cadena s pNew


moverDer :: Linea -> Linea
moverDer Vacia = Vacia
moverDer (Cadena s p) =
    if p >= (length s)
        then (Cadena s (length s))
        else
            let pNew = p + 1
            in (Cadena s pNew)


moverIni :: Linea -> Linea
moverIni Vacia = Vacia
moverIni (Cadena s p) = (Cadena s 0)


moverFin :: Linea -> Linea
moverFin Vacia = Vacia
moverFin (Cadena s p) =
    let pNew = (length s)
    in (Cadena s pNew)


insertar :: Char -> Linea -> Linea
insertar c Vacia = (Cadena [c] 1)
insertar c (Cadena s p) =
    if p < 0 || p > (length s)
        then (Cadena s (length s))
        else
            let (inicioString, finalString) = splitAt p s
            in (Cadena (inicioString ++ [c] ++ finalString) (p + 1))


borrar :: Linea -> Linea
borrar Vacia = Vacia
borrar (Cadena s p) = 
    if p <= 0 || p > (length s)
        then (Cadena s (length s))
        else
            let (inicioString, finalString) = splitAt p s
            in (Cadena ((init inicioString) ++ finalString) (p - 1))