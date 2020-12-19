{-# LANGUAGE GADTSyntax #-}

module Lang where

import Control.Monad.State

data Exp where
  Var :: Variable -> Exp
  Cons :: Constructor -> [Exp] -> Exp
  Call :: CallType -> [Exp] -> Exp
  Case :: Exp -> [(Pattern, Exp)] -> Exp
  Let :: [(Variable, Exp)] -> Exp -> Exp
  deriving (Eq, Show)

data Pattern = Pat Constructor [Variable] deriving (Eq, Show)

newtype Variable = VarName String deriving (Eq, Show)

data CallType
  = Func Function
  | Builtin Builtin
  | Undef Function
  deriving (Eq, Show)

newtype Function = FuncName String deriving (Eq, Show)

newtype Builtin = BuiltinName String deriving (Eq, Show)

newtype Constructor = ConsName String deriving (Eq, Show)

data Program = Program {
  progExpr :: Exp,
  progDefs :: [Definition]
 } deriving (Eq, Show)

data Definition = Definition Function [Variable] Exp deriving (Eq, Show)


builtinsList = ["<", "+", ">"]