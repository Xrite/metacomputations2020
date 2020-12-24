{-# LANGUAGE GADTSyntax #-}

module Lang where

import Control.Monad.State
import Data.Text.Prettyprint.Doc

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

data Program = Program
  { progExpr :: Exp,
    progDefs :: [Definition]
  }
  deriving (Eq, Show)

data Definition = Definition Function [Variable] Exp deriving (Eq, Show)

instance Pretty Exp where
  pretty exp = case exp of
    Var v -> pretty v
    Cons c es -> pretty c <> tupledOrNone es
    Call (Func f) args -> pretty f <> tupled (map pretty args)
    Call (Builtin b) args -> pretty b <> tupled (map pretty args)
    Call (Undef f) args -> pretty f <> tupled (map pretty args)
    Case e ps ->
      let start = pretty "case" <+> pretty e <+> pretty "of"
          printCase (pat, body) = pretty pat <+> pretty "->" <+> pretty body
       in nest 4 (vsep (start : (map printCase ps)))
    Let bs e ->
      let printBinding (v, e) = pretty v <+> pretty "=" <+> pretty e
          start = pretty "let" <+> hsep (punctuate comma (map printBinding bs)) <+> pretty "in"
       in nest 4 (vsep [start, pretty e])

tupledOrNone :: Pretty a => [a] -> Doc ann
tupledOrNone es = case es of
  [] -> pretty ""
  _ -> tupled (map pretty es)

instance Pretty Variable where
  pretty (VarName v) = pretty v

instance Pretty Constructor where
  pretty (ConsName c) = pretty c

instance Pretty Function where
  pretty (FuncName f) = pretty f

instance Pretty Builtin where
  pretty (BuiltinName b) = pretty b

instance Pretty Pattern where
  pretty (Pat c vs) = pretty c <> tupledOrNone vs

instance Pretty Program where
  pretty (Program e defs) = vsep (pretty e : pretty "where" : map pretty defs)

instance Pretty Definition where
  pretty (Definition f args e) = pretty f <> tupled (map pretty args) <+> pretty "=" <+> pretty e

builtinsList = ["*", "+", ">"]
