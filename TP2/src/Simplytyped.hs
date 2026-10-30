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

{-
data LamTerm    =  LVar String
                |  LAbs String Type LamTerm
                |  LApp LamTerm LamTerm


-}
-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' [] (LVar s)        = Free (Global s)
conversion' ns (LVar s)        = case dist ns s of
                                       Just n  -> Bound n
                                       Nothing -> Free (Global s)  
conversion' ns (LAbs s t l)    = Lam t (conversion' (s : ns) l)
conversion' ns (LApp l1 l2)    = conversion' ns l1 :@: conversion' ns l2
conversion' ns (LLet s l1 l2)  = Let (conversion' ns l1) (conversion' (s:ns) l2)
conversion' ns (LZero)         = Zero
conversion' ns (LSuc t)        = Suc (conversion' ns t)
conversion' ns (LRec t1 t2 t3) = Rec (conversion' ns t1) (conversion' ns t2) (conversion' ns t3)
conversion' ns (LNil)          = Nil
--Sabemos que los parentesis no van pero los dejamos por una cuestion estetica
conversion' ns (LCons t1 t2)   = Cons (conversion' ns t1) (conversion' ns t2)

dist :: [String] -> String -> Maybe Int
dist [] _     = Nothing
dist (x:xs) s = if x == s 
                then (Just 0) 
                else case dist xs s of
                          Nothing -> Nothing
                          Just n -> Just (n+1)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
-- i cantidad de abstracciones
-- t termino a sustituir
-- 's' el tercer argumento es donde se va a sustituir
-- (...)[t/s]
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub _ t Zero                  = Zero
sub i t (Suc t')              = Suc (sub i t t')
sub i t (Rec t1 t2 t3)        = (Rec (sub i t t1) (sub i t t2) (sub i t t3))
sub i t Nil                   = Nil
sub i t (Cons n lv)           = Cons (sub i t n) (sub i t lv)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f)       = Lam t f
quote (VNum nv)        = quoteNumVal nv
quote (VList l)        = quoteList l

quoteNumVal :: NumVal -> Term
quoteNumVal NZero        = Zero
quoteNumVal (NSuc nv)    = Suc (quoteNumVal nv)

quoteList :: ListVal -> Term
quoteList VNil        = Nil
quoteList (VCons n l) = Cons (quoteNumVal n) (quoteList l)

-- evalúa un término en un entorno dado

eval :: NameEnv Value Type -> Term -> Value
eval nvs t@(Free s)                   = case lookup s nvs of
                                           Just (u,t') -> u
                                           Nothing     -> error("que haces aca") 
eval nvs (Let t1 t2)                  = let v = eval nvs t1
                                        in eval nvs (sub 0 (quote v) t2) 
eval nvs (t1 :@: t2)                  = let (VLam t u) = eval nvs t1
                                            v2 = eval nvs t2
                                        in eval nvs $ sub 0 (quote v2) u 
eval nvs (Zero)                       = VNum NZero
eval nvs (Suc t)                      = VNum $ NSuc $ unNum $ eval nvs t
eval nvs (Nil)                        = VList VNil
eval nvs (Cons t1 t2)                 = let n = unNum $ eval nvs t1
                                            t2' = unList $ eval nvs t2
                                        in VList $ VCons n t2'
eval nvs (Rec t1 t2 Nil)              = eval nvs t1
eval nvs (Rec t1 t2 (Cons n lv))      = eval nvs (t2 :@: n :@: (Rec t1 t2 lv))
eval nvs (Rec t1 t2 Zero)             = eval nvs t1
eval nvs (Rec t1 t2 (Suc t))          = eval nvs (t2 :@: (Rec t1 t2 t) :@: t) 
eval nvs (Rec t1 t2 t3)               = let t3' = eval nvs t3 in eval nvs (Rec t1 t2 (quote t3'))
eval _ (Lam t f)                      = VLam t f

unNum :: Value -> NumVal 
unNum (VNum x) = x

unList :: Value -> ListVal
unList (VList x) = x 

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
-- Entorno global para variables libres 
-- Contexto para variables ligadas
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
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt -> infer' (tt:c) e t2
infer' c e Zero = ret NatT
infer' c e (Suc t) = infer' c e t >>= \tt -> case tt of 
                                                  NatT -> ret NatT
                                                  _   -> notfunError tt
infer' c e (Rec t1 t2 t3) = infer' c e t1 >>= \tt -> infer' c e t2 >>= \tu -> infer' c e t3 >>= \tv ->
  case tv of
    NatT -> (case tu of
              (FunT ty1 (FunT ty2 ty3)) -> if (ty2 == tv) 
                                           then (if (tt == ty1 && ty1 == ty3) 
                                                 then ret tt 
                                                 else matchError (FunT tt (FunT tv tt)) tu)
                                           else matchError ty2 tv
              _ -> notfunError tv)
    ListT -> (case tu of
                (FunT ty1 (FunT ty2 ty3)) -> if (ty1 == NatT) 
                                             then (if (tt == ty2 && ty2 == ty3) 
                                                   then ret tt
                                                   else matchError (FunT tv (FunT tt tt)) tu)
                                             else matchError ty1 NatT
                _ -> notfunError tv)
    _ -> notfunError tv   
infer' c e Nil = ret ListT
infer' c e (Cons t1 t2) = infer' c e t1 >>= \tt -> infer' c e t2 >>= \tu -> if (tt == NatT) 
                                                                            then (if tu == ListT then ret tu else matchError ListT tu)
                                                                            else matchError NatT tt 
-- if (t1' == tt && tt == t3' && tv == NatT ) then ret tt else matchError
