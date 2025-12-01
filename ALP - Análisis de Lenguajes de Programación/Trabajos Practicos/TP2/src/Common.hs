module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Los tipos del cálculo implementado son dados por la siguiente gramática:
  -- T ::= E | T -> T | Nat | List Nat
  -- Tipo de los tipos
  data Type = EmptyT 
            | FunT Type Type
            -- Sección 8
            | NatT
            | ListT
            deriving (Show, Eq)
  
  -- Una vez definido los tipos, se pueden definir los términos:
  -- t ::= x | λx : T. t | t t | 0 | Suc t | Rec t t t | nil | cons t t | RL t t t
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  LAbs String Type LamTerm
                |  LApp LamTerm LamTerm
                -- Sección 8
                |  LLet String LamTerm LamTerm
                -- naturales 
                |  LZero
                |  LSuc LamTerm
                |  LRec LamTerm LamTerm LamTerm
                -- listas
                |  LNil
                |  LCons LamTerm LamTerm
                |  LRecL LamTerm LamTerm LamTerm
                deriving (Show, Eq)

  -- Términos localmente sin nombres
  data Term  = Bound Int -- variable ligadas por índice De Bruijn
             | Free Name -- variables libres conservan el nombre
             | Term :@: Term
             | Lam Type Term
             -- Sección 8
             | Let Term Term
             -- naturales
             | Zero
             | Suc Term
             | Rec Term Term Term
             -- listas
             | Nil
             | Cons Term Term
             | RecL Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             -- Sección 8
             | VNum NumVal
             | VList ListVal
             
           deriving (Show, Eq)

  -- Valores Numericos
  data NumVal = NZero | NSuc NumVal deriving (Show, Eq)

  -- Listas de números
  data ListVal = VNil | VCons NumVal ListVal deriving (Show, Eq)
   
  -- Contextos del tipado
  type Context = [Type]
