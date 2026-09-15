module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict  as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor var s = case M.lookup var s of 
                     Just n  -> n
                     Nothing -> 0 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s 
stepComm (Seq Skip c) s = c :!: s 
stepComm (Let v e) s    = let (e0 :!: s') = evalExp e s 
                          in Skip :!: update v e0 s'
stepComm (IfThenElse p0 c0 c1) s = let (b0 :!: _) = evalExp p0 s
                                       c = if b0 then c0 else c1
                                    in c :!: s
stepComm cmd@(RepeatUntil c p0) s = (Seq c (IfThenElse p0 Skip cmd)) :!: s
stepComm (Seq c0 c1) s  = let ( c0' :!: s' ) = stepComm c0 s
                          in (Seq c0' c1 :!: s')

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s   = n :!: s
evalExp (Var v )  s   = lookfor v s :!: s
evalExp (UMinus x) s  = let (n :!:s')   = evalExp x s in (-n) :!: s' 
evalExp (Plus x y) s  = let (n0 :!: s')  = evalExp x s 
                            (n1 :!: s'') = evalExp y s'
                        in (n0 + n1) :!: s''
evalExp (Minus x y) s = let (n0 :!: s')  = evalExp x s 
                            (n1 :!: s'') = evalExp y s'
                        in (n0 - n1) :!: s''
evalExp (Times x y) s = let (n0 :!: s')  = evalExp x s 
                            (n1 :!: s'') = evalExp y s'
                        in (n0 * n1) :!: s''
evalExp (Div x y) s   = let (n0 :!: s')  = evalExp x s 
                            (n1 :!: s'') = evalExp y s'
                        in (div n0 n1) :!: s''
evalExp (VarInc x) s  = let n0 = lookfor x s
                            n  = n0 + 1
                        in n :!: update x n s
evalExp (VarDec x) s  = let n0 = lookfor x s
                            n  = n0 - 1
                        in n :!: update x n s 

evalExp (BTrue) s     = True :!: s
evalExp (BFalse) s    = False :!: s
evalExp (Lt e0 e1) s  = let (n0 :!: s')  = evalExp e0 s 
                            (n1 :!: s'') = evalExp e1 s'
                        in (n0 < n1) :!: s''
evalExp (Gt e0 e1) s  = let (n0 :!: s')  = evalExp e0 s 
                            (n1 :!: s'') = evalExp e1 s'
                        in (n0 > n1) :!: s''
evalExp (Eq e0 e1) s  = let (n0 :!: s')  = evalExp e0 s 
                            (n1 :!: s'') = evalExp e1 s'
                        in (n0 == n1) :!: s''
evalExp (NEq e0 e1) s = let (n0 :!: s')  = evalExp e0 s 
                            (n1 :!: s'') = evalExp e1 s'
                        in (n0 /= n1) :!: s''
evalExp (And p0 p1) s = let (b0 :!: s')  = evalExp p0 s 
                            (b1 :!: s'') = evalExp p1 s'
                        in (b0 && b1) :!: s''
evalExp (Or p0 p1) s  = let (b0 :!: s')  = evalExp p0 s 
                            (b1 :!: s'') = evalExp p1 s'
                        in (b0 || b1) :!: s''
evalExp (Not p0) s    = let (b0 :!: s') = evalExp p0 s
                        in (not b0) :!: s'