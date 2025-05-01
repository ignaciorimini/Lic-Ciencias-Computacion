-- Definimos el tipo de datos Exp (expresiones aritméticas)
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

-- Función principal: recibe un string y construye una Exp
parseRPN :: String -> Exp
parseRPN s = parseS [] s  -- Llama a parseS con una pila vacía y el string

-- Función auxiliar que va procesando la pila y el string
parseS :: [Exp] -> String -> Exp
parseS ps "" =  -- Caso base: si el string está vacío
    if length ps == 1 
        then head ps   -- Si la pila tiene un solo elemento, ese es el resultado final
        else error "Error: expresión mal formada."  -- Si no, error
parseS ps s = case x of  -- x es el primer "token" del string
    "+" -> opTop Add ps s'   -- Si es "+", aplicar la suma
    "-" -> opTop Sub ps s'   -- Si es "-", aplicar la resta
    "*" -> opTop Prod ps s'  -- Si es "*", aplicar el producto
    "/" -> opTop Div ps s'   -- Si es "/", aplicar la división
    n   -> parseS (Lit (read n) : ps) s'  -- Si no es un operador, debe ser un número: lo leemos y lo ponemos en la pila
  where (x, s') = getFirst s  -- Separamos el primer token x y el resto del string s'

-- Función para aplicar un operador sobre los dos elementos del tope de la pila.
-- Recibe un constructor de Exp que es una operación (no es Lit), la pila de Exp y el string.
-- Aplica la operación a los dos elementos tope de la pila y pone dicha operación en la pila, llamando otra vez a parseS (recursión mutua).
opTop :: (Exp -> Exp -> Exp) -> [Exp] -> String -> Exp
opTop op (x:y:ps) s = parseS ((op y x) : ps) s  
-- Tomamos los dos primeros elementos de la pila (cuidado: el segundo de la pila es el operando izquierdo)
-- Aplicamos la operación, y ponemos el resultado de vuelta en la pila
opTop _ _ _ = error "Error: expresión mal formada."
-- Si no hay suficientes elementos en la pila para operar, error

-- Función que extrae el primer token del string (número o símbolo)
getFirst :: String -> (String, String)
getFirst s = aux "" (deleteSpace s)
  where 
    -- Elimina espacios al principio del string
    deleteSpace (' ':s) = deleteSpace s
    deleteSpace s = s

    -- Auxiliar para separar el primer token
    aux p "" = (reverse p, "")  -- Si llegamos al final, devolvemos el token que íbamos armando
    aux p (' ':s) = (reverse p, deleteSpace s)  -- Si encontramos espacio, terminamos el token actual
    aux [] (c:s) | c `elem` "+-*/" = ([c], deleteSpace s)
    -- Si no habíamos empezado a armar nada y encontramos un operador (+ - * /), lo devolvemos como token
    aux p (c:s) = aux (c:p) s  
    -- Si es un caracter de un número, lo agregamos al token y seguimos
