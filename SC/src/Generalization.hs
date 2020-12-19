{-# LANGUAGE FlexibleContexts #-}
module Generalization where

import Lang

import Substitution
import NameGen

import Control.Monad.State

data Generalization = Generalization Exp Substitution Substitution
  deriving (Show)


mostSpecificGeneralization e1 e2 = do 
    commonSubexpression <$> commonFunctor e1 e2

commonFunctor e1 e2
  | Var v1 <- e1, Var v2 <- e2, v1 == v2 = return $ Generalization (Var v1) empty empty
  | Call f1 es1 <- e1,
    Call f2 es2 <- e2,
    f1 == f2 = do
    gens <- sequence $ zipWith commonFunctor es1 es2
    let es = map (\(Generalization e _ _) -> e) gens
    let th1s = map (\(Generalization _ th _) -> th) gens
    let th2s = map (\(Generalization _ _ th) -> th) gens
    let e = Call f1 es
    return $ Generalization e (unionAll th1s) (unionAll th2s)
  | otherwise = do
    v <- freshVar
    return $ Generalization (Var v) (single v e1) (single v e2)


pairs (Generalization e s1 s2) = [(v1, v2) | v1 <- vars, v2 <- vars, v1 /= v2, assoc s1 v1 == assoc s1 v2, assoc s2 v1 == assoc s2 v2]
  where
    vars = domain s1

pair gen = case pairs gen of
  [] -> Nothing
  p : _ -> Just p

commonSubexpression gen@(Generalization e s1 s2) =
  case pair gen of
    Nothing -> gen
    Just (v1, v2) -> Generalization (apply (single v1 (Var v2)) e) (remove s1 v1) (remove s2 v1)
  
getMSG e1 e2 = runState (mostSpecificGeneralization e1 e2) (initialNameGen "v" "f")