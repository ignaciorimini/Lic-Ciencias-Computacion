module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
-- Debe eliminar todas las variables ligadas reemplazándolas por índices De Bruijn.
conversion :: LamTerm -> Term
conversion = conv []

-------------------
-- conv: función auxiliar que toma un contexto de nombres ligados y un LamTerm, y lo convierte en un Term con índices de De Bruijn sustituyendo nombres por Bound n o Free.
-- El argumento [String] es una lista donde guardamos los nombres de las variables ligadas introducidas por λ en el camino hacia el nodo actual.
-- λx. λy. λz. ... -> ctx = ["z","y","x"]
-- "z" tiene índice 0
-- "y" tiene índice 1
-- "x" tiene índice 2

-- LVar.
-- Si la variable aparece en el contexto, es ligada (Bound n); si no está, es una variable libre (Free).
-- elemIndex devuelve Just n si x está en ctx en la posición n; esa n es el índice de De Bruijn (0 = λ más cercano).
-- Si elemIndex devuelve Nothing, x no está ligada por ningún λ, por lo tanto se convierte en Free (variable libre).

-- LAbs.
-- Al entrar en una abstracción λx, agregamos x al contexto.
-- x pasa a ser la variable ligada más cercana (índice 0).

-- LApp.
-- Simplemente convertimos ambos sub-términos usando el mismo contexto.

-- LLet local t1 t2.
-- Un término "let x = t1 in t2" se trata como azúcar sintáctico para 
-- una aplicación de una lambda: (λx. t2) t1.
-- En la representación sin nombres, esto se implementa como Let u v.
--   u = el valor que se liga (t1)
--   v = el cuerpo donde la variable local es visible (t2)
-- Para convertir:
--   1) Convertimos t1 con el contexto actual (las variables ligadas antes del let).
--   2) Para convertir t2, agregamos la variable local al contexto,
--      porque dentro de t2, `local` se comporta igual que una variable ligada por λ.
--      Esto significa que `local` corresponde al índice De Bruijn 0 dentro de t2.

conv :: [String] -> LamTerm -> Term
conv ctx lt = case lt of
  LVar x -> case elemIndex x ctx of
    Just n -> Bound n
    Nothing -> Free (Global x)
  
  LAbs x t body -> Lam t (conv (x:ctx) body)

  LApp t1 t2 -> conv ctx t1 :@: conv ctx t2

  LLet local t1 t2 -> Let (conv ctx t1) (conv (local:ctx) t2)

  LZero -> Zero
  LSuc t -> Suc (conv ctx t)
  LRec t1 t2 t3 -> Rec (conv ctx t1) (conv ctx t2) (conv ctx t3)

  LNil -> Nil
  LCons t1 t2 -> Cons (conv ctx t1) (conv ctx t2)
  LRecL t1 t2 t3 -> RecL (conv ctx t1) (conv ctx t2) (conv ctx t3)

----------------------------
--- evaluador de términos
----------------------------

-- Sustituye en el término u la variable ligada con índice i por el término t
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i + 1) t t2)
sub _ _ Zero                  = Zero
sub i t (Suc n)               = Suc (sub i t n)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)
sub _ _ Nil                   = Nil
sub i t (Cons t1 t2)          = Cons (sub i t t1) (sub i t t2)
sub i t (RecL t1 t2 t3)       = RecL (sub i t t1) (sub i t t2) (sub i t t3)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f)      = Lam t f
quote (VNum NZero)    = Zero
quote (VNum (NSuc n)) = Suc $ quote (VNum n)
quote (VList VNil) = Nil
quote (VList (VCons n ns)) = Cons (quote (VNum n)) (quote (VList ns))

-- evalúa un término en un entorno dado
-- Debe evaluar un término representado con De Bruijn, devolviendo un Value.
eval :: NameEnv Value Type -> Term -> Value
eval env t = case t of
  -- Variables ligadas (no deberían aparecer en términos cerrados).
  Bound i -> error "No se pueden evaluar términos con variables ligadas."

  -- Variables libres.
  Free n -> case lookup n env of
    Just (v, _) -> v
    Nothing -> error $ "Variable libre no encontrada en el entorno: " ++ show n

  -- Abstracciones.
  t1 :@: t2 -> case eval env t1 of
    -- Operador es lambda.
    VLam _ f -> 
      let arg = eval env t2
          termArg = quote arg
          newBody = sub 0 termArg f      
      in eval env newBody

    _ -> error "No se puede aplicar."

  Let t1 t2 ->
    let v   = quote (eval env t1)
        t2' = sub 0 v t2
    in eval env t2'

  -- Aplicaciones.
  -- Devuelve la función como valor. 
  Lam t f -> VLam t f 

  Zero -> VNum NZero

  Suc t1 ->
    let v = eval env t1
    in case v of
      (VNum n) -> VNum (NSuc n)
      _        -> error "`suc` aplicada a un valor no númerico"

  Rec t1 t2 t3 ->
    case eval env t3 of
      VNum NZero -> eval env t1
      VNum (NSuc n) ->
        eval env (t2 :@: (Rec t1 t2 t3') :@: t3')
        where t3' = quote (VNum n)
  
  Nil -> VList VNil

  Cons t1 t2 -> case (eval env t1, eval env t2) of
    (VNum n, VList ns) -> VList (VCons n ns)
    _                 -> error "cons aplicado a valores inválidos"

  RecL t1 t2 t3 ->
      case eval env t3 of
        VList VNil -> eval env t1
        VList (VCons n ns) ->
          let headVal  = quote (VNum n)      
              tailVal  = quote (VList ns)      
              recCall  = RecL t1 t2 tailVal    
          in eval env (t2 :@: headVal :@: tailVal :@: recCall)
              

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
-- Context (lista de Type) contiene los tipos de variables ligadas (De Bruijn) en orden: cabeza = índice 0.
-- NameEnv tiene las variables globales (nombre -> (Value,Type)).
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)

infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t

infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt

infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu

infer' c e (Let t1 t2) = infer' c e t1 >>= \tt1 -> infer' (tt1 : c) e t2 

infer' c e Zero = ret NatT

infer' c e (Suc t) = infer' c e t >>= \tt ->
  case tt of
    NatT -> ret NatT
    _    -> matchError NatT tt

infer' c e (Rec t1 t2 t3) = 
  infer' c e t1 >>= \tt1 -> 
  infer' c e t2 >>= \tt2 ->
  infer' c e t3 >>= \tt3 ->
  case tt3 of
      NatT -> do
        let expected_t2 = FunT tt1 (FunT NatT tt1)
        if (tt2 == expected_t2)
          then ret tt1 
          else matchError expected_t2 tt2 
      _ -> matchError NatT tt3 

infer' c e (Nil) = ret (ListT)

infer' c e (Cons t1 t2) =
  infer' c e t1 >>= \tt1 ->
  infer' c e t2 >>= \tt2 ->
    case (tt1, tt2) of
      (NatT, ListT) -> ret ListT
      _            -> matchError (FunT NatT (FunT ListT ListT)) (FunT tt1 tt2)

infer' c e (RecL t1 t2 t3) = 
  infer' c e t1 >>= \tt1 -> 
  infer' c e t2 >>= \tt2 ->
  infer' c e t3 >>= \tt3 ->
  case tt3 of
      ListT -> do
        let expected_t2 = FunT NatT (FunT ListT (FunT tt1 tt1))
        if (tt2 == expected_t2)
          then ret tt1 
          else matchError expected_t2 tt2 
      _ -> matchError ListT tt3 

  