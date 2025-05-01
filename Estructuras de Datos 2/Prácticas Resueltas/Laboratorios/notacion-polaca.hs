-- Evaluador de notación polaca.
np :: String -> Int
np xs = let (r, rest) = np_aux' xs
        in if null rest
            then r
            else error ("Error: basura al final: " ++ rest)

-- Esta función calcula una expresión entera y devuelve el resto de la cadena sin procesar.
np_aux' :: String -> (Int, String)
np_aux' [] = error "Error: esperando más caracteres"
np_aux' (x:xs)  | isOperator x = 
                    let (n, xs') = np_aux' xs
                        (m, ys) = np_aux' xs'
                    in (operator x n m, ys)
                | isDigit x = 
                    let (ns, xs') = span isDigit (x:xs)
                    in (read ns, xs')
                | isSpace x = np_aux' xs
                | otherwise = error ("Error: encontré un " ++ [x])

-- Funciones auxiliares.
isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _ = False

operator :: Char -> Int -> Int -> Int
operator '+' = (+)
operator '-' = (-)
operator '*' = (*)
operator '/' = div
operator _ = error "not an operator"

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
