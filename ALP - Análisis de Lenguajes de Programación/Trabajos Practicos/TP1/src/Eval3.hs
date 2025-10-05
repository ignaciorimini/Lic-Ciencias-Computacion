module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
-- Primer componente es el mapa de variables y el segundo es la traza.
type State = (M.Map Variable Int, String)

-- Estado vacío.
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado.
lookfor :: Variable -> State -> Either (Error, String) Int
lookfor v (m, t) = case M.lookup v m of
  Just n  -> Right n
  Nothing -> Left (UndefVar, t)

-- Cambia el valor de una variable en un estado.
update :: Variable -> Int -> State -> State
update v n (m, t) = (M.insert v n m, t)

-- Agrega una traza dada al estado.
addTrace :: String -> State -> State
addTrace msg (m, t) = (m, t ++ msg ++ " ")

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either (Error, String) State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either (Error, String) State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado.
stepComm :: Comm -> State -> Either (Error, String) (Pair Comm State)
stepComm c st = case c of
  Skip -> Right (Skip :!: st)

  Let v e -> do
    n :!: st' <- evalExp e st 
    let st'' = update v n st'
    let st''' = addTrace (v ++ ":=" ++ show n) st''
    Right (Skip :!: st''')

  Seq Skip c1 -> Right (c1 :!: st)
  Seq c0 c1 -> do
    c0' :!: st' <- stepComm c0 st
    Right (Seq c0' c1 :!: st')

  IfThenElse b c0 c1 -> do
    bv :!: st' <- evalExp b st
    Right (case bv of
      True  -> c0 :!: st'
      False -> c1 :!: st')

  RepeatUntil c b -> Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: st)

-- Evalúa una expresión.
evalExp :: Exp a -> State -> Either (Error, String) (Pair a State)
evalExp e st@(_, t) = case e of
  (Const n) -> Right (n :!: st)
  
  (Var v) -> do
    n <- lookfor v st 
    Right (n :!: st)
  
  (VarInc v) -> do
    n <- lookfor v st
    let n' = n + 1
    let st' = update v n' st
    let st'' = addTrace (v ++ ":=" ++ show n') st'
    Right (n' :!: st'')

  (UMinus e) -> do
    n :!: st' <- evalExp e st
    Right ((-n) :!: st')

  (Plus e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 + n1) :!: st'')

  (Minus e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 - n1) :!: st'')

  (Times e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 * n1) :!: st'')

  (Div e0 e1) -> do
    n0 :!: st'@(_, t') <- evalExp e0 st
    n1 :!: st''@(_, t'') <- evalExp e1 st'
    if n1 == 0
      then Left (DivByZero, t'')
      else Right ((n0 `div` n1) :!: st'')

  BTrue -> Right (True :!: st)

  BFalse -> Right (False :!: st)

  (Lt e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 < n1) :!: st'')
  
  (Gt e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 > n1) :!: st'')

  (And p0 p1) -> do
    b0 :!: st' <- evalExp p0 st
    b1 :!: st'' <- evalExp p1 st'
    Right ((b0 && b1) :!: st'')

  (Or p0 p1) -> do
    b0 :!: st' <- evalExp p0 st
    b1 :!: st'' <- evalExp p1 st'
    Right ((b0 || b1) :!: st'')

  (Not p) -> do
    b :!: st' <- evalExp p st
    Right ((not b) :!: st')
  
  (Eq e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 == n1) :!: st'')

  (NEq e0 e1) -> do
    n0 :!: st' <- evalExp e0 st
    n1 :!: st'' <- evalExp e1 st'
    Right ((n0 /= n1) :!: st'')
