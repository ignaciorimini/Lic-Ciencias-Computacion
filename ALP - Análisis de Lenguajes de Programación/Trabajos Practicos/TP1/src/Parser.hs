module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
    , reservedOpNames = [ "+"
                        , "++"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , ":"
                        ]
    }
  )


-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp    = intterm   `chainl1` addop
intterm   = intfactor `chainl1` mulop
intfactor = parens lis intexp <|> negint <|> nat <|> var

addop = 
  do { reservedOp lis "+"; return Plus } <|> 
  do { reservedOp lis "-"; return Minus }

mulop =
  do { reservedOp lis "*"; return Times } <|>
  do { reservedOp lis "/"; return Div }

negint = do
  reservedOp lis "-"
  ie <- intexp
  return (UMinus ie)

var = do
  v <- identifier lis
  inc <- optionMaybe (reservedOp lis "++")
  case inc of
    Just _  -> return (VarInc v)
    Nothing -> return (Var v)

nat = do
  n <- natural lis
  return (Const (fromInteger n))

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp  = boolterm   `chainl1` orP
boolterm = boolfactor `chainl1` andP
boolfactor =
  (parens lis boolexp)
  <|> negbool
  <|> boolval
  <|> comp

andP = do
  reservedOp lis "&&"
  return And

orP = do
  reservedOp lis "||"
  return Or

comp = do
  op1 <- intexp
  op  <- compop
  op2 <- intexp
  return (op op1 op2)

compop =
  do { reservedOp lis "==" ; return Eq } <|>
  do { reservedOp lis "!="; return NEq} <|>
  do { reservedOp lis ">" ; return Gt } <|>
  do { reservedOp lis "<" ; return Lt }

boolval =
  do { reservedOp lis "true"; return BTrue } <|>
  do { reservedOp lis "false"; return BFalse }

negbool = do
  reservedOp lis "!"
  be <- boolexp
  return (Not be)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commterm seqop
commterm = skipP <|> ifP <|> repP <|> caseP <|> assignP

skipP = do
  reserved lis "skip"
  return Skip

assignP = do
  lhs <- identifier lis
  reservedOp lis "="
  rhs <- intexp
  return (Let lhs rhs)

seqop = do
  reservedOp lis ";"
  return Seq

ifP = do
  reserved lis "if"
  cond <- boolexp
  tb <- braces lis comm
  eb <- optionMaybe (do {reserved lis "else" ; braces lis comm })  
  case eb of
    Just e  -> return (IfThenElse cond tb e)
    Nothing -> return (IfThen cond tb)   

repP = do
  reserved lis "repeat"
  body <- braces lis comm
  reserved lis "until"
  cond <- boolexp
  return (RepeatUntil body cond)

caseP = do
  reserved lis "case"
  l <- braces lis (many c)
  return (Case l)
  where
    c = do
      cond <- boolexp
      reservedOp lis ":"
      act <- braces lis comm
      return (cond, act)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)