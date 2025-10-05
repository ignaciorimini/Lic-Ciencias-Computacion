import Parsing
import Control.Applicative hiding (many)

-- Escribir un transformador que al recibir un parser, devuelva un nuevo parser que se comporta como el original pero que también acepta opcionalmente que las cadenas estén entre paréntesis.
trss :: Parser a -> Parser a
trss p = 
    do  symbol "("
        t <- trss p
        symbol ")"
        return t
    <|> do  t <- p
            return t

-- Ejemplos.
-- parse (trss natural) "(42)" -> [(42,"")]
-- parse (trss natural) "((42))" -> [(42,"")]
-- parse (trss natural) "((42)))" -> [(42,")")]
-- parse (trss natural) "((42))))" -> [(42,"))")]
-- parse (trss natural) "(((42))))" -> [(42,")")]
-- parse (trss natural) "((((42))))" -> [(42,"")]