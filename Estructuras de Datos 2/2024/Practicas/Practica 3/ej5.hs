{-
La notación polaca inversa o RPN (del inglés Reverse Polish Notation) es una manera alternativa de escribir
expresiones matemáticas, en la cual los operadores se escriben luego de los operandos, es decir, usando notación posfija. 
Por ejemplo, la suma de los enteros 3 y 5 que con notación infija notamos 3 + 5, en RPN se escribe 3 5 +

Para evaluar una expresión escrita en RPN, podemos usar un stack o pila. Recorremos la expresión de izquierda
a derecha. Cada vez que encontramos un número, lo apilamos. Cada vez que encontramos un operador, retiramos
los dos números que estan en la cima de la pila, le aplicamos el operador y apilamos el resultado. Si la expresión
está bien formada, al alcanzar el final de la misma, debemos tener un único número en la pila, que representa el
resultado de la expresión.

Una de las ventajas de esta notación es que elimina la necesidad del uso de paréntesis y de reglas de precedencia
de operadores, ya que el proceso de apilamiento determina el orden en que deben computarse las operaciones.

a) Defina una función 
parseRPN :: String → Exp 
que, dado un string que representa una expresión escrita en RPN, construya un elemento del tipo Exp presentado en el ejercicio 4 
correspondiente a la expresión dada. Por ejemplo: parseRPN "8 5 3 − 3 ∗ +"" = Add (Lit 8) (Prod (Sub (Lit 5) (Lit 3)) (Lit 3))
Ayuda: para implementar parseRPN puede seguir un algoritmo similar al presentado anteriormente. En lugar
de evaluar las expresiones, debe construir un valor de tipo Exp.

b) Defina una función 
evalRPN :: String → Int
para evaluar expresiones aritméticas escritas en RPN. Por ejemplo: evalRPN “8 5 3 − 3 ∗ +” = 14
Ayuda: use las funciones parseRPN y eval definidas anteriormente
-}

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

parseRPN :: String -> Exp
parseRPN s = 
    let entradas = words s
        pila = [head words:]
    in  | last pila == "+"  = take 2 (reverse pila)
        | last pila == "-"  =
        | last pila == "*"  =
        | last pila == "/"  =
        | otherwise         = parseRPN tail s