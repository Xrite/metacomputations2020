module Embedding where

import Lang

byDiving e1 e2
  | Call f2 es2 <- e2, any (embedding e1) es2 = True
  | Cons c2 es2 <- e2, any (embedding e1) es2 = True
  | otherwise = False

byCoupling e1 e2
  | Call f1 es1 <- e1, Call f2 es2 <- e2, f1 == f2, length es1 == length es2, and $ zipWith embedding es1 es2 = True
  | Cons c1 es1 <- e1, Cons c2 es2 <- e2, c1 == c2, length es1 == length es2, and $ zipWith embedding es1 es2 = True
  | otherwise = False

byVariables e1 e2
  | Var v1 <- e1, Var v2 <- e2 = True
  | otherwise = False

embedding e1 e2 = byVariables e1 e2 || byCoupling e1 e2 || byDiving e1 e2
