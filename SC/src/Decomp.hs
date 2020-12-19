module Decomp where

import Builtins
import Control.Applicative
import Data.Maybe
import Lang

data Decomposition
  = Observable Exp
  | WithContext Exp Context
  deriving (Eq, Show)

data Context
  = ContextTop
  | ContextCase Context [(Pattern, Exp)]
  deriving (Eq, Show)

up (exp, ctx) = case ctx of
  ContextTop -> (exp, ctx)
  ContextCase ctx' ps -> ((Case exp ps), ctx')

push ContextTop ps = ContextCase ContextTop ps
push (ContextCase ctx' ps') ps = ContextCase (push ctx' ps) ps'

decompose exp = fromMaybe (error $ "Unable to decompose expression " ++ show exp) $ maybeObs <|> maybeCon
  where
    maybeObs = Observable <$> obs exp

    maybeCon = uncurry WithContext <$> con exp

    obs exp
      | Var v <- exp = Just exp
      | Call (Undef f) args <- exp = Just exp
      | Call (Builtin b) args <- exp, Nothing <- unfoldBuiltin b args = Just exp
      | Cons c es <- exp = Just exp
      | otherwise = Nothing

    con exp
      | Just r <- red exp = Just (r, ContextTop)
      | Case e ps <- exp, Just (r, ctx) <- con e = Just (r, push ctx ps)
      | otherwise = Nothing

    red exp
      | Call (Func f) args <- exp = Just exp
      | Call (Builtin b) args <- exp, Just _ <- unfoldBuiltin b args = Just exp
      | Case e ps <- exp, Just _ <- obs e = Just exp
      | otherwise = Nothing

compose (Observable e) = e
compose (WithContext e ctx) = case ctx of
  ContextTop -> e
  ContextCase ctx' ps -> compose (WithContext (Case e ps) ctx')
