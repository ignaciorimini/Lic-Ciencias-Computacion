{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones, aritméticas y booleanas
data Exp a where
  
  -- Expresiones enteras
  Const  :: Int -> Exp Int
  Var    :: Variable -> Exp Int
  VarInc :: Variable -> Exp Int
  UMinus :: Exp Int -> Exp Int
  Plus   :: Exp Int -> Exp Int -> Exp Int
  Minus  :: Exp Int -> Exp Int -> Exp Int
  Times  :: Exp Int -> Exp Int -> Exp Int
  Div    :: Exp Int -> Exp Int -> Exp Int

  -- Expresiones booleanas
  BTrue  :: Exp Bool
  BFalse :: Exp Bool
  Lt     :: Exp Int -> Exp Int -> Exp Bool
  Gt     :: Exp Int -> Exp Int -> Exp Bool
  And    :: Exp Bool -> Exp Bool -> Exp Bool
  Or     :: Exp Bool -> Exp Bool -> Exp Bool
  Not    :: Exp Bool -> Exp Bool
  Eq     :: Exp Int -> Exp Int -> Exp Bool
  NEq    :: Exp Int -> Exp Int -> Exp Bool  

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- Comandos (sentencias)
-- Observar que sólo se permiten variables de tipo entero
data Comm
  = Skip
  | Let Variable (Exp Int)
  | Seq Comm Comm
  | IfThenElse (Exp Bool) Comm Comm
  | RepeatUntil Comm (Exp Bool)
  deriving (Show, Eq)

pattern IfThen :: Exp Bool -> Comm -> Comm
pattern IfThen b c = IfThenElse b c Skip

pattern Case :: [(Exp Bool, Comm)] -> Comm
pattern Case branches <- (caseMatcher -> Just branches)
  where
    Case = caseBuilder

caseBuilder :: [(Exp Bool, Comm)] -> Comm
caseBuilder []         = Skip
caseBuilder [(c,b)]    = IfThenElse c b Skip
caseBuilder ((c,b):xs) = IfThenElse c b (caseBuilder xs)

caseMatcher :: Comm -> Maybe [(Exp Bool, Comm)]
caseMatcher Skip = Just []
caseMatcher (IfThenElse c t Skip) = Just [(c, t)]
caseMatcher (IfThenElse c t e) = 
  case caseMatcher e of 
    Nothing -> Nothing
    Just xs -> Just ((c,t):xs)
caseMatcher _ = Nothing

data Error = DivByZero | UndefVar deriving (Eq, Show)
