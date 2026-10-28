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
conversion' [] (LVar s)       = Free (Global s)
conversion' ns (LVar s)       = case dist ns s of
                                      Just n  -> Bound n
                                      Nothing -> Free (Global s)  
conversion' ns (LAbs s t l)   = Lam t (conversion' (s : ns) l)
conversion' ns (LApp l1 l2)   = conversion' ns l1 :@: conversion' ns l2
conversion' ns (LLet s l1 l2) = Let (conversion' ns l1) (conversion' (s:ns) l2)

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

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
{-
type NameEnv v t = [(Name, (v, t))]
data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
-}

eval :: NameEnv Value Type -> Term -> Value
eval nvs t@(Free s)         = case lookup s nvs of
                                 Just (u,t') -> VLam t' (quote u)
                                 Nothing     -> VLam EmptyT t 
eval nvs (Let t1 t2)        = let v = eval nvs t1
                              in eval nvs (sub 0 t2 (quote v)) 
eval nvs ((Lam t u) :@: (Lam t' u')) = eval nvs (sub 0 u u')
eval nvs (t1 :@: t2)        = let v1 = eval nvs t1 
                                  v2 = eval nvs t2
                              in eval nvs (quote v1 :@: quote v2)
eval _ (Lam t f)            = VLam t f


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


