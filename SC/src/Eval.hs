module Eval where

import Builtins
import Data.List (find)
import Lang
import Substitution
eval (Program e defs) = eval e
  where
    eval exp = case exp of
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
      _ -> exp

