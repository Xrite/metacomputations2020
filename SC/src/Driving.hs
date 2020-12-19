{-# LANGUAGE FlexibleContexts #-}

module Driving where

import Lang
import Decomp
import Substitution

import Builtins

import ProcessTree

import Debug.Trace

driving (Let bs body) defs = body : map snd bs
driving expr defs = case decompose expr of
  Observable (Var v) -> []
  Observable (Call (Undef f) args) -> args
  Observable (Call (Builtin b) args) -> args
  Observable (Cons c es) -> es
  WithContext (Call (Func f) args) ctx -> [compose (WithContext (unfold defs f args) ctx)]
  WithContext (Call (Builtin b) args) ctx -> 
      case unfoldBuiltin b args of
          Just e -> [compose (WithContext e ctx)]
          Nothing -> args
  WithContext (Case (Cons c es) ps) ctx ->
    let (pat, body) = matchPattern ps (Cons c es)
        Pat _ vs = pat
        subst = bindAll vs es
        newBody = apply subst body
     in [compose (WithContext newBody ctx)]
  WithContext (Case (Var v) ps) ctx ->
    let mkCons (Pat c vs) = Cons c $ map Var vs
        mkBranch (pat, body) = compose (WithContext (apply (single v $ mkCons pat) body) ctx)
        branches = map mkBranch ps
     in Var v : branches
  WithContext (Case (Call (Undef f) args) ps) ctx ->
    let mkBranch (pat, body) = compose (WithContext body ctx)
        branches = map mkBranch ps
     in Call (Undef f) args : branches
  WithContext (Case (Call (Builtin b) args) ps) ctx ->
    let mkBranch (pat, body) = compose (WithContext body ctx)
        branches = map mkBranch ps
     in Call (Builtin b) args : branches
  x -> error $ show x

drive node = do
    expr <- getExpression node
    --traceM $ "Driving " ++ show (decompose expr)
    defs <- getDefinitions 
    let next = driving expr defs
    addChildren node next
