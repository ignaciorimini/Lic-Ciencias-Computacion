-- Definimos el tipo de datos Exp (expresiones aritméticas)
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

---------------------------------------------------
-- a) Definición de parseRPN :: String -> Exp que, dado un string que representa una expresión escrita en RPN, construya un elemento del tipo Exp presentado en el ejercicio 4 correspondiente a la expresión dada.
parseRPN :: String -> Exp
parseRPN s = parseAux [] (words s)

-- Recibe una pila y una lista de "tokens" (caracteres) y computa la expresión que representa la lista de tokens.
parseAux :: [Exp] -> [String] -> Exp
parseAux [e] [] = e -- Caso final: no quedan más tokens y hay una única expresión.
parseAux _ [] = error "Error: expresión mal formada."
parseAux pila (x:xs) 
    -- Si es un dígito, lo convertimos en Lit y lo apilamos.
    | isDigit x = parseAux ((Lit (read x)):pila) xs
    -- Si es un operador, aplicar el operador.
    | elem x ["+","-","*","/"] = aplicarOp x pila xs
    | otherwise = error "Error: expresión mal formada."

-- Recibe un string que representa una operación, la convierte al constructor Exp y la aplica a los dos elementos del tope de la pila.
-- Luego, remueve los dos elementos, el operador y apila la expresión formada, llamando nuevamente a parseAux (recursión mutua).
aplicarOp :: String -> [Exp] -> [String] -> Exp
aplicarOp op (x:y:restoPila) tokens = 
    let operador = case op of
                    "+" -> Add
                    "-" -> Sub
                    "*" -> Prod
                    "/" -> Div
    in parseAux ((operador y x):restoPila) tokens
aplicarOp _ _ _ = error "Error: no hay suficientes operandos."

-- Recibe un string de un solo caracter, hace pattern matching para obtener dicho caracter y devuelve True si es un dígito, False en caso contrario.
isDigit :: [Char] -> Bool
isDigit (c:xs) = c >= '0' && c <= '9'
isDigit _ = False

-- Ejemplo y caso de prueba.
-- parseRPN "8 5 3 - 3 * +"
-- Add (Lit 8) (Prod (Sub (Lit 5) (Lit 3)) (Lit 3))

---------------------------------------------------
-- b) Definición de función evalRPN :: String -> Int para evaluar expresiones aritméticas escritas en RPN.

eval :: Exp -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = div (eval e1) (eval e2)

evalRPN :: String -> Int
evalRPN s = eval (parseRPN s)

-- Ejemplo y caso de prueba.
-- evalRPN "8 5 3 - 3 * +"
-- 8 + 3*(5 - 3) = 8 + 3*2 = 8 + 6 = 14