module Eval where

import Builtins
import Data.List (find)
import Lang
import Substitution

eval :: Program -> Exp
eval (Program e defs) = eval e
  where
    eval exp = case exp of
      Var x -> error $ "Residual variable in program " ++ show x
      Cons c es ->
        let es' = map eval es
         in Cons c es'
      Call (Func f) args ->
        eval (unfold defs f args)
      Call (Builtin b) args ->
        let es = map eval args
         in evalBuiltin b es
      Call (Undef f) args ->
        error $ "Can't eval undef function " ++ show f
      Case (Cons c es) ps ->
        let (Pat cp vs, body) = matchPattern ps (Cons c es)
         in eval $ apply (bindAll vs es) body
      Case e ps ->
        eval (Case (eval e) ps)
      Let vs e ->
        let vs' = map (\(v, body) -> (v, eval body)) vs
            e' = apply (fromBindings vs') e
         in eval e'

evalWithVars :: Substitution -> Program -> Exp
evalWithVars subst (Program e defs) = eval $ Program e' defs'
  where
    e' = apply subst e
    defs' = map modifyDef defs
    modifyDef (Definition f vs e) = Definition f vs (apply subst e)
