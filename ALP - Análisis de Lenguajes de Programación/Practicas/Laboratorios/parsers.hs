import Prelude hiding ((>>=), return)

-- Definición del tipo de datos de un parser. 
-- Es una función que toma un string y devuelve una lista con la entrada consumida y el resto sin consumir.
-- Lista vacía = indica una falla (no se pudo parsear).
-- Lista unitaria = indica éxito.
type Parser a = String -> [(a, String)]

-- ___________________________________________________________________________________
-- PARSERS BÁSICOS.

-- El parser que siempre tiene éxito, no consume la entrada y devuelve un valor dado.
return :: a -> Parser a
return v = \s -> [(v, s)]

-- El parser que siempre falla.
failure :: Parser a
failure = \_ -> []

-- El parser que devuelve el 1er caracter de su entrada o falla si ésta es vacía.
item :: Parser Char
item (x:xs) = [(x, xs)]
item [] = []

-- Función auxiliar para aplicar un parser a una cadena.
parse :: Parser a -> String -> [(a, String)]
parse p s = p s

-- Ejemplos.
-- parse (return 1) "abc" -> [(1, "abc")]
-- parse failure "abc" -> []
-- parse item "abc" -> [("a", "bc")]
-- parse item "" -> []


-- ___________________________________________________________________________________
-- COMBINADORES DE PARSERS.

-- Combinador CHOICE.
-- Si p tiene éxito nos quedamos con su resultado.
-- Si p falla, aplicamos el parser q.
(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) s = case p s of
                [] -> parse q s
                [(v, out)] -> [(v, out)]

-- Ejemplos.
-- (item <|> return '1') "abc" -> [('a', "bc")]
-- (item <|> return '1') "" -> [(1, "")]


------------------------------
-- Combinador SECUENCIAMIENTO.
-- 1. Ejecuta un parser p.
-- 2. Usa el valor que devolvió (v).
-- 3. Construye a partir de él un nuevo parser con f v.
-- 4. Continua el parseo sobre la entrada restante.
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(p >>= f) s = case parse p s of
                [] -> []
                [(v, out)] -> parse (f v) out

-- Ejemplo: aplica 3 parsers y devuelve el resultado del 1ero y 3ero en un par.
-- Sin utilizar notación DO.
p :: Parser (Char, Char)
p = item >>= \x ->
    item >>= \_ ->
    item >>= \y ->
    return (x, y)

-- Utilizando notación DO: hace falta declarar Parser como mónada.
-- p :: Parser (Char, Char)
-- p = do  x <- item
--         item
--         y <- item
--         return (x, y)

-- parse p "abcdef" -> [('a', 'c'), "def"]
-- parse p "ab" -> []


-- ___________________________________________________________________________________
-- PRIMITIVAS DERIVADAS.

-- sat. Parser con predicado.
-- Parsea un caracter solo si satisface una condición booleana.
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else failure

-- Utilizando notación DO: hace falta declarar Parser como mónada.
-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do  x <- item
--             if p x  then return x
--                     else failure

------------------------------
-- Parser de dígitos.
-- Utilizando sat podemos parsear si un caracter en una cadena es un dígito.
digit :: Parser Char
digit = sat isDigit
    where
        isDigit :: Char -> Bool
        isDigit c = c >= '0' && c <= '9'

-- Ejemplos.
-- digit "abc" -> []
-- digit "3abc" -> [('3', "abc")]

------------------------------
-- Parser de caracter dado.
-- Utilizando sat podemos parsear si un caracter de una cadena es igual a un caracter específico.
char :: Char -> Parser Char
char x = sat (x ==)

-- Ejemplos.
-- char 'a' "abc" -> [('a', "bc")]
-- char 'a' "bac" -> []

------------------------------
-- Parser de una cadena dada.
-- Este parser intenta reconocer una cadena específica dentro de la entrada.
string :: String -> Parser String
string [] = return []
string (x:xs) = char x >>= \_ ->
                string xs >>= \_ ->
                return (x:xs)

-- Utilizando notación DO: hace falta declarar Parser como mónada.
-- string :: String -> Parser String
-- string [] = return []
-- string (x:xs) = do  char x
--                     string xs
--                     return (x:xs)

-- Ejemplos.
-- string "abc" "aaabc" -> []
-- strin "abc" "abcdef" -> [("abc", "def")]

------------------------------
-- Aplican un parser muchas veces hasta que fallan, devolviendo los valores parseados en una lista.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p =   p >>= \v ->
            many p >>= \vs ->
            return (v:vs)

-- Utilizando notación DO: hace falta declarar Parser como mónada.
-- many1 :: Parser a -> Parser [a]
-- many1 p = do    v <- p
--                 vs <- many p
--                 return (v:vs)

------------------------------
-- Parsea un número natural.
nat :: Parser Int
nat =   many1 digit >>= \xs ->
        return (read xs)

-- Utilizando notación DO: hace falta declarar Parser como mónada.
-- nat :: Parser Int
-- nat = do    xs <- many1 digit
--             return (read xs)

-- Ejemplos.
-- nat "123" -> [(123, "")]
-- nat "abc123" -> []

------------------------------
-- Parsea 0 o más espacios.
space :: Parser ()
space = do  many (sat isSpace)
            return ()

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'