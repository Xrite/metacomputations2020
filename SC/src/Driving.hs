{-# LANGUAGE FlexibleContexts #-}

module Driving where

import Builtins
import Control.Monad.State
import Decomp
import Lang
import ProcessTree
import Substitution
import Unfolding

--import Debug.Trace
import Debug.Pretty.Simple

driving (Let bs body) defs = return $ body : map snd bs
driving expr defs = case decompose expr of
  Observable (Var v) -> return []
  Observable (Call (Undef f) args) -> return args
  Observable (Call (Builtin b) args) -> return args
  Observable (Cons c es) -> return es
  WithContext (Call (Func f) args) ctx -> do
    unfolded <- renameUnfold defs f args
    return [compose (WithContext unfolded ctx)]
  WithContext (Call (Builtin b) args) ctx ->
    case unfoldBuiltin b args of
      Just e -> return [compose (WithContext e ctx)]
      Nothing -> return args
  WithContext (Case (Cons c es) ps) ctx ->
    let (pat, body) = matchPattern ps (Cons c es)
        Pat _ vs = pat
        subst = bindAll vs es
        newBody = apply subst body
     in return [compose (WithContext newBody ctx)]
  WithContext (Case (Var v) ps) ctx ->
    let mkCons (Pat c vs) = Cons c $ map Var vs
        mkBranch (pat, body) = compose (WithContext (apply (single v $ mkCons pat) body) ctx)
        branches = map mkBranch ps
     in return $ Var v : branches
  WithContext (Case (Call (Undef f) args) ps) ctx ->
    let mkBranch (pat, body) = compose (WithContext body ctx)
        branches = map mkBranch ps
     in return $ Call (Undef f) args : branches
  WithContext (Case (Call (Builtin b) args) ps) ctx ->
    let mkBranch (pat, body) = compose (WithContext body ctx)
        branches = map mkBranch ps
     in return $ Call (Builtin b) args : branches
  x -> error $ show x

drive node = do
  expr <- getExpression node
  --pTraceM $ "Driving " ++ show expr
  defs <- getDefinitions
  next <- lift $ driving expr defs
  addChildren node next
