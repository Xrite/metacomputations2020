{-# LANGUAGE FlexibleContexts #-}

module Unfolding where

import Data.List
import Lang
import NameGen
import Substitution
import Control.Monad.State
import NameGen

renameUnfold :: (Foldable t, MonadState NameGen m) => t Definition -> Function -> [Exp] -> m Exp
renameUnfold defs f args = case find (\(Definition f' _ _) -> f == f') defs of
  Nothing -> error $ "Can't unfold. No such function " ++ show f
  Just (Definition f' vs e) -> do
    e' <- renameExp e
    let subst = bindAll vs args
    let e'' = apply subst e'
    return e''
  where
    renameExp e = case e of
      Var x -> return $ Var x
      Cons c es -> do
        es' <- mapM renameExp es
        return $ Cons c es'
      Call f args -> do
        args' <- mapM renameExp args
        return $ Call f args'
      Case e ps -> do
        ps' <- mapM renameCase ps
        e' <- renameExp e
        return $ Case e' ps'
      Let bs e -> error "Let unsupported"
    renameCase (pat, body) = do
      let (Pat c vs) = pat
      vs' <- freshVars (length vs)
      let body' = apply (bindAll vs (map Var vs')) body
      return (Pat c vs', body')
