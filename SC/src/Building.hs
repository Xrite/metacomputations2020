{-# LANGUAGE FlexibleContexts #-}

module Building where

import Control.Monad.State
import Lang

define f args body = modify (++ [def])
  where
    def = Definition (FuncName f) (map VarName args) body

mkProgram builder = Program e defs
  where
    (e, defs) = runState builder []

case_ exp patterns = Case exp st
  where
    st = execState patterns []

of_ c vs body = modify (++ [(pat, body)])
  where
    pat = Pat (ConsName c) (map VarName vs)

var v = Var (VarName v)

app f args 
  | elem f builtinsList = Call (Builtin (BuiltinName f)) args
  | otherwise = Call (Func (FuncName f)) args

app0 f = app f []

appU f args = Call (Undef (FuncName f)) args

cons c args = Cons (ConsName c) args

cons0 c = cons c []

example = mkProgram $ do
  define "not" ["x"] $
    case_ (var "x") $ do
      of_ "True" [] $ cons0 "False"
      of_ "False" [] $ cons0 "True"
  define "and" ["x", "y"] $
    case_ (var "x") $ do
      of_ "True" [] $
        case_ (var "y") $ do
          of_ "True" [] $ cons0 "True"
          of_ "False" [] $ cons0 "False"
      of_ "False" [] $ cons0 "False"
  return $
    app "and" [cons0 "True", app "not" [cons0 "False"]]

