module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados.
type State = M.Map Variable Int

-- Estado vacío.
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado.
lookfor :: Variable -> State -> Int
lookfor v st = case M.lookup v st of
                  Just n  -> n
                  Nothing -> error "Variable no definida"

-- Cambia el valor de una variable en un estado o la agrega si no estaba definida.
update :: Variable -> Int -> State -> State
update v n st = M.insert v n st

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip.
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado. Devuelve un par nuevoComando :!: nuevoEstado.
stepComm :: Comm -> State -> Pair Comm State
stepComm c st = case c of
  Skip -> Skip :!: st

  Let v e -> 
    let n :!: st' = evalExp e st 
    in Skip :!: update v n st'

  Seq Skip c1 -> c1 :!: st
  Seq c0 c1 -> 
    let c0' :!: st' = stepComm c0 st 
    in Seq c0' c1 :!: st'

  IfThenElse b c0 c1 -> 
    let bv :!: st' = evalExp b st
    in case bv of
      True  -> c0 :!: st'
      False -> c1 :!: st'

  RepeatUntil c b -> Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: st
   

-- Evalúa una expresión.
evalExp :: Exp a -> State -> Pair a State
evalExp e st = case e of
  (Const n) -> n :!: st
  
  (Var v) -> lookfor v st :!: st
  
  (VarInc v) -> 
    let n = lookfor v st
    in (n + 1) :!: update v (n + 1) st

  (UMinus e) ->
    let n :!: st' = evalExp e st
    in (-n) :!: st'

  (Plus e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 + n1) :!: st''

  (Minus e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 - n1) :!: st''

  (Times e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 * n1) :!: st''

  (Div e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 `div` n1) :!: st''

  BTrue -> True :!: st

  BFalse -> False :!: st

  (Lt e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 < n1) :!: st''
  
  (Gt e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 > n1) :!: st''

  (And p0 p1) ->
    let b0 :!: st' = evalExp p0 st
        b1 :!: st'' = evalExp p1 st'
    in (b0 && b1) :!: st''

  (Or p0 p1) ->
    let b0 :!: st' = evalExp p0 st
        b1 :!: st'' = evalExp p1 st'
    in (b0 || b1) :!: st''

  (Not p) ->
    let b :!: st' = evalExp p st
    in (not b) :!: st'
  
  (Eq e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 == n1) :!: st''

  (NEq e0 e1) ->
    let n0 :!: st' = evalExp e0 st
        n1 :!: st'' = evalExp e1 st'
    in (n0 /= n1) :!: st''
