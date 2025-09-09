import Prelude hiding ((>>=), return)

-- ___________________________________________________________________________________
-- Consideremos la siguiente gramática libre de contexto.
-- exp -> term '+' exp | term
-- term -> atom '*' term | atom
-- atom -> '(' exp ')' | n
-- n -> d | dn
-- d -> '0' | '1' | ... | '9'

-- Simplificamos la gramática factorizando las reglas para expr y term.
-- exp -> term ('+' exp | e)
-- term -> atom ('*' term | e)

-- Definimos el parser para la gramática.
expr :: Parser Int
expr = 
    term >>= \t ->
      ((symbol "+" >>= \_ ->
        expr >>= \e ->
        return (t + e))
       <|> return t)

term :: Parser Int
term =
    factor >>= \f ->
      ((symbol "*" >>= \_ ->
        term >>= \t ->
        return (f * t))
       <|> return f)

factor :: Parser Int
factor =
    (symbol "(" >>= \_ ->
     expr >>= \e ->
     symbol ")" >>= \_ ->
     return e)
    <|> natural


eval :: String -> Int
eval xs = fst (head (parse expr xs))

-- Ejemplos.
-- eval "2*3+4" -> 10
-- eval "2*(3+4)" -> 14
-- eval "2*(3+4)+10" -> 24
-- eval "2*(3+4)+(10*2)" -> 34

-- ___________________________________________________________________________________
-- Definición del tipo de datos de un parser. 
-- Es una función que toma un string y devuelve una lista con la entrada consumida y el resto sin consumir.
-- Lista vacía = indica una falla (no se pudo parsear).
-- Lista unitaria = indica éxito.
type Parser a = String -> [(a, String)]


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

-- Combinador CHOICE.
(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) s = case p s of
                [] -> parse q s
                [(v, out)] -> [(v, out)]

-- Combinador SECUENCIAMIENTO.
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(p >>= f) s = case parse p s of
                [] -> []
                [(v, out)] -> parse (f v) out

-- sat. Parser con predicado.
-- Parsea un caracter solo si satisface una condición booleana.
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else failure

-- Parser de dígitos.
digit :: Parser Char
digit = sat isDigit
    where
        isDigit :: Char -> Bool
        isDigit c = c >= '0' && c <= '9'

-- Parser de caracter dado.
char :: Char -> Parser Char
char x = sat (x ==)

-- Parser de una cadena dada.
string :: String -> Parser String
string [] = return []
string (x:xs) = char x >>= \_ ->
                string xs >>= \_ ->
                return (x:xs)

-- Aplican un parser muchas veces hasta que fallan, devolviendo los valores parseados en una lista.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p =   p >>= \v ->
            many p >>= \vs ->
            return (v:vs)

-- Parsea un número natural.
nat :: Parser Int
nat =   many1 digit >>= \xs ->
        return (read xs)

-- Parsea 0 o más espacios.
space :: Parser ()
space = do  many (sat isSpace)
            return ()

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'

-- Parsear ignorando los espacios.
token :: Parser a -> Parser a
token p = 
        space >>= \_ ->
        p >>= \v ->
        space >>= \_ ->
        return v

-- Parsea una cadena ignorando espacios.
symbol :: String -> Parser String
symbol xs = token (string xs)

-- Parsea un natural ignorando espacios.
natural :: Parser Int
natural = token nat

-- Parser de lista de naturales.
listnat :: Parser [Int]
listnat =
        symbol "[" >>= \_ ->
        natural >>= \n ->
        many (symbol "," >>= \_ -> natural) >>= \ns ->
        symbol "]" >>= \_ ->
        return (n:ns)