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

zero = cons0 "Z"
one = cons "S" [zero]
two = cons "S" [one]
three = cons "S" [two]

true = cons0 "True"
false = cons0 "False"

makeStr [] = cons0 "Nil"
makeStr (x:xs) = cons "Cons" [cons0  [x], makeStr xs]