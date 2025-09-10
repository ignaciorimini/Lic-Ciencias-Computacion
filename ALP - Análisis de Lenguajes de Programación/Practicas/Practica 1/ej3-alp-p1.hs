import Parsing
import Control.Applicative hiding (many)

{-
trs :: Parser a -> Parser a
trs p = do many (symbol "(")
           t <- p
           many (symbol ")")
           return t
        <|> do t <- p
               return t
-}

trs :: Parser a -> Parser a
trs p = p'