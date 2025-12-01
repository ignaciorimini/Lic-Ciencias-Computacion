{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

-------------------------------------------------------------------------------
-- DECLARACIONES

-- Indica que el parser correrá en el monad P, usando thenP y returnP.
-- Permite manejo de errores y secuenciación monádica en el parser.
%monad {P} {thenP} {returnP}

-- Genera la función parseStmt cuyo símbolo inicial es el no-terminal Def.
-- Se usa para parsear una única definición.
%name parseStmt Def

-- Genera la función parseStmts con símbolo inicial Defs.
-- Se usa para parsear múltiples definiciones en un archivo o REPL.
%name parseStmts Defs

-- Genera la función term que empieza a parsear desde el no-terminal Exp.
-- Se usa para parsear un único término lambda.
%name term Exp

-- Indica que el lexer produce valores del tipo Token.
-- El parser usará ese tipo para recibir la secuencia de tokens.
%tokentype {Token}

-- Usa la función lexer para obtener tokens desde la entrada.
-- TEOF se usa como token de fin de archivo.
%lexer {lexer} {TEOF}

-- Declaración de los posibles tokens.
%token
    '='     { TEquals }
    ':'     { TColon }
    '\\'    { TAbs }
    '.'     { TDot }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    VAR     { TVar $$ }
    TYPEE   { TTypeE }
    TYPENAT { TTypeNat }
    TYPEL   { TTypeList }
    DEF     { TDef }
    LET     { TLet }
    IN      { TIn }
    ZERO    { TZero }
    SUC     { TSuc }
    REC     { TRec }
    RECL    { TRecL }
    NIL     { TNil }
    CONS    { TCons }
    
-- Declaración de precedencias.
%left '=' 
%right '->'
%right '\\' '.' LET IN
%right REC
%right RECL
%right CONS
%right SUC

-------------------------------------------------------------------------------
-- GRAMÁTICA
-- En Happy, los símbolos de una producción tienen posiciones: $1, $2, $3, etc.
-- Cada $i representa el valor parseado del i-ésimo símbolo en la regla.
-- Ejemplo:  Defexp : DEF VAR '=' Exp   { Def $2 $4 }
-- $1 = DEF, 
-- $2 = VAR, 
-- $3 = '=', 
-- $4 = Exp. 
-- La acción usa esos valores para construir el AST.

%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LLet $2 $4 $6 }
        | REC Atom Atom Exp            { LRec $2 $3 $4}
        | RECL Atom Atom Exp           { LRecL $2 $3 $4 }
        | SUC Exp                      { LSuc $2 }
        | CONS Atom Exp                { LCons $2 $3 }
        | NAbs                         { $1 }
        
NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }  
        | '(' Exp ')'                  { $2 }
        | ZERO                         { LZero }
        | NIL                          { LNil }

Type    : TYPEE                        { EmptyT }
        | TYPENAT                      { NatT }
        | TYPEL TYPENAT                { ListT }
        | Type '->' Type               { FunT $1 $3 }
        | '(' Type ')'                 { $2 }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }


{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TTypeE
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TLet
               | TIn
               | TZero
               | TSuc
               | TRec
               | TTypeNat
               | TTypeList
               | TNil
               | TCons
               | TRecL
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    ('0':cs) -> cont TZero cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("def",rest)  -> cont TDef rest
                              ("let",rest)  -> cont TLet rest
                              ("in",rest)   -> cont TIn rest
                              ("Nat",rest)  -> cont TTypeNat rest
                              ("List",rest) -> cont TTypeList rest
                              ("suc",rest)  -> cont TSuc rest
                              ("R",rest)    -> cont TRec rest
                              ("nil",rest)  -> cont TNil rest
                              ("cons",rest) -> cont TCons rest
                              ("RL",rest)   -> cont TRecL rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
