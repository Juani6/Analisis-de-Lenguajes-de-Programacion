module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor var s = case M.lookup var s of 
                     Nothing -> (Left UndefVar) 
                     Just n  -> (Right n)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                   = Right $ Skip :!: s 
stepComm (Seq Skip c) s           = Right $ c :!: s 
stepComm cmd@(RepeatUntil c p0) s = Right $ (Seq c (IfThenElse p0 Skip cmd)) :!: s
stepComm (Let v e) s              = case evalExp e s of
                                         (Right (e0 :!: s')) -> Right $ Skip :!: update v e0 s'
                                         Left err -> Left err 
stepComm (IfThenElse p0 c0 c1) s  = case evalExp p0 s of
                                         (Right (b0 :!: _))-> let c = if b0 then c0 else c1 in Right $ c :!: s
                                         Left err -> Left err 
stepComm (Seq c0 c1) s            = case stepComm c0 s of
                                         (Right (c0' :!: s')) -> Right $ (Seq c0' c1) :!: s'
                                         Left err -> Left err
                         


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s   = Right (n :!: s)
evalExp (Var v ) s    = case lookfor v s of
                               (Right n)  -> Right (n :!: s) 
                               Left err -> Left err
evalExp (UMinus x) s  = case evalExp x s of
                             (Right (n :!: s'))  -> Right ((-n) :!: s') 
                             Left err -> Left err  
evalExp (Plus x y) s  = case evalExp x s of
                             Right (n0 :!: s') -> case evalExp y s' of
                                                  (Right (n1 :!: s'')) -> Right $ (n0 + n1) :!: s''
                                                  Left err -> Left err
                             Left err -> Left err
evalExp (Minus x y) s  = case evalExp x s of
                             Right (n0 :!: s') -> case evalExp y s' of
                                                  (Right (n1 :!: s'')) -> Right $ (n0 - n1) :!: s''
                                                  Left err -> Left err
                             Left err -> Left err
evalExp (Times x y) s  = case evalExp x s of
                             Right (n0 :!: s') -> case evalExp y s' of
                                                  (Right (n1 :!: s'')) -> Right $ (n0 * n1) :!: s''
                                                  Left err -> Left err
                             Left err -> Left err
evalExp (Div x y) s  = case evalExp x s of
                            Right (n0 :!: s') -> case evalExp y s' of
                                                  (Right (n1 :!: s'')) -> if n1 == 0 
                                                                          then Left DivByZero
                                                                          else Right $ (div n0 n1) :!: s''
                                                  Left err -> Left err
                            Left err -> Left err
evalExp (VarInc x) s  = case lookfor x s of
                             Right n0 -> let n = n0 + 1 in Right (n :!: update x n s)
                             Left err -> Left err
evalExp (VarDec x) s  = case lookfor x s of
                             Right n0 -> let n = n0 - 1 in Right (n :!: update x n s)
                             Left err -> Left err
evalExp (BTrue) s     = Right (True :!: s)
evalExp (BFalse) s    = Right (False :!: s)
evalExp (Lt e0 e1) s  = case evalExp e0 s of
                             Right (n0 :!: s') -> case evalExp e1 s' of
                                                         Right (n1 :!: s'') -> Right $ (n0 < n1) :!: s'' 
                                                         Left err -> Left err
                             Left err -> Left err
evalExp (Gt e0 e1) s  = case evalExp e0 s of
                             Right (n0 :!: s') -> case evalExp e1 s' of
                                                         Right (n1 :!: s'') -> Right $ (n0 > n1) :!: s'' 
                                                         Left err -> Left err
                             Left err -> Left err
evalExp (Eq e0 e1) s  = case evalExp e0 s of
                             Right (n0 :!: s') -> case evalExp e1 s' of
                                                         Right (n1 :!: s'') -> Right $ (n0 == n1) :!: s'' 
                                                         Left err -> Left err
                             Left err -> Left err
evalExp (NEq e0 e1) s  = case evalExp e0 s of
                             Right (n0 :!: s') -> case evalExp e1 s' of
                                                         Right (n1 :!: s'') -> Right $ (n0 /= n1) :!: s'' 
                                                         Left err -> Left err
                             Left err -> Left err
evalExp (And p0 p1) s  = case evalExp p0 s of
                              Right (b0 :!: s') -> case evalExp p1 s' of
                                                         Right (b1 :!: s'') -> Right $ (b0 && b1) :!: s'' 
                                                         Left err -> Left err
                              Left err -> Left err
evalExp (Or p0 p1) s  = case evalExp p0 s of
                              Right (b0 :!: s') -> case evalExp p1 s' of
                                                         Right (b1 :!: s'') -> Right $ (b0 || b1) :!: s'' 
                                                         Left err -> Left err
                              Left err -> Left err
evalExp (Not p0) s    = case evalExp p0 s of
                             Right (b0 :!: s') -> Right $ (not b0) :!: s'
                             Left err -> Left err
                             