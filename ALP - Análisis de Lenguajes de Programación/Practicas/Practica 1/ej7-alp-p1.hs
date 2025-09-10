import Parsing
import Control.Applicative

-- Notas.
-- Parsers basicos: failure, item, return, do.
-- return = pure del archivo Parsing.lhs
-- El do aplica un parser, que devuelve [a, String] y el String se lo pasa a la segunda linea (segundo parser). Si se usa la flecha (->) se guarda el valor a en una variable. 
-- Es decir, do t <- parser guarda en t = a, y el string lo pasa al segundo parser.

data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype deriving Show

-- "Int -> Char -> Float" -> Fun DInt (Fun DChar DFloat)
-- "(Int -> Char) -> Float" -> Fun (Fun DInt DChar) DFloat

-- Reglas
--ht -> bt | bt ’->’ ht
--bt -> ’Int’ | ’Char’ | ’Float’

-- Reescribimos
--ht -> tp (e | ’->’ ht)
--bt -> ’Int’ | ’Char’ | ’Float’
--tp -> bt | ’(’ ht ’)’

ht :: Parser Hasktype
ht = do t <- tp
        (do 
            symbol "->"
            h <- ht
            return (Fun t h)
         <|> return t)

tp :: Parser Hasktype
tp = bt <|> paren

bt :: Parser Hasktype
bt = pInt <|> pChar <|> pFloat
    where
        pInt = do 
                symbol "Int"
                return DInt
        
        pChar = do 
                symbol "Char"
                return DChar
        
        pFloat = do 
                    symbol "Float"
                    return DFloat

paren :: Parser Hasktype
paren = do 
            symbol "("
            t <- ht
            symbol ")"
            return t

eval :: String -> Hasktype
eval xs = fst (head (parse ht xs))

---------------------------------------
{-bt :: Parser Hasktype
bt = do s <- string "Int" 
        return s
    <|> 
    do s <- string "Char" 
        return s
    <|> 
    do s <- string "Float" 
        return s
    

tp :: Parser Hasktype
tp =  bt <|>
        do symbol "("
        h <- ht
        symbol ")"
        return h
-}