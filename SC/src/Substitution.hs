module Substitution where
import Lang 

import qualified Data.List as List
import Data.Maybe

import Control.Monad

data Substitution = Substitution [(Variable, Exp)] deriving (Eq, Show)

union (Substitution s1) (Substitution s2) = Substitution $ List.union s1 s2

unionAll substs = foldl union empty substs

empty = Substitution []

single v e = Substitution [(v, e)]

assoc (Substitution s) v = lookup v s

add (Substitution s) v e = Substitution $ (v, e) : s

remove (Substitution s) v = case lookup v s of
    Nothing -> Substitution s
    Just e -> Substitution $ List.delete (v, e) s

bindAll vs es = Substitution $ zip vs es

idSubst vs = bindAll vs (map Var vs)

bindings (Substitution s) = s

domain (Substitution s) = map fst s

--apply (Substitution s) exp = foldl (\e (v', e') -> substitute e v' e') exp s
apply s exp = case exp of
    Var v -> case assoc s v of
        Nothing -> Var v
        Just e -> e
    Cons c es -> Cons c $ map (apply s) es
    Call f es -> Call f $ map (apply s) es
    Case e ps -> Case (apply s e) $ map (\(p', e') -> (p', apply s e')) ps
    Let vs e0 -> Let (map (\(v, e) -> (v, apply s e)) vs) (apply s e0)

substitute exp var to = case exp of
  Var v -> if v == var then to else Var v
  Cons c es -> Cons c $ map (\e -> substitute e var to) es
  Call f es -> Call f $ map (\e -> substitute e var to) es
  Case e ps -> Case (substitute e var to) $ map (\(p_i, e_i) -> (p_i, substitute e_i var to)) ps
  Let vs e0 -> Let (map (\(v, e) -> (v, substitute e var to)) vs) (substitute e0 var to)

renaming e1 e2
  | Var a <- e1, Var b <- e2 = Just [(a, b)]
  | Cons c1 es1 <- e1,
    Cons c2 es2 <- e2,
    c1 == c2,
    length es1 == length es2 = do
    rs <- zipWithM renaming es1 es2
    if check (concat rs)
      then return $ List.nub (concat rs)
      else Nothing
  | Call f1 es1 <- e1,
    Call f2 es2 <- e2,
    f1 == f2,
    length es1 == length es2 = do
    rs <- zipWithM renaming es1 es2
    if check (concat rs)
      then return $ List.nub (concat rs)
      else Nothing
  | otherwise = Nothing
  where
    check rs = and [x1 == x2 && y1 == y2 || x1 /= x2 && y1 /= y2 | (x1, y1) <- rs, (x2, y2) <- rs]

isRenaming e1 e2 = isJust $ renaming e1 e2

fromRenaming ren = let (from, to) = unzip ren in bindAll from (map Var to)

invRenaming ren = let (from, to) = unzip ren in zip to from

