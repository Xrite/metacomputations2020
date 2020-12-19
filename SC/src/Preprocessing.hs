{-# LANGUAGE FlexibleContexts #-}

module Preprocessing where

import Control.Monad.State
import Lang
import NameGen
import Substitution

preprocessProgram (Program e defs) = Program <$> preprocessExp e <*> mapM preprocessDef defs
  where
    preprocessExp e = case e of
      Var x -> return $ Var x
      Cons c es -> do
        es' <- mapM preprocessExp es
        return $ Cons c es'
      Call f args -> do
        args' <- mapM preprocessExp args
        return $ Call f args'
      Case e ps -> do
        ps' <- mapM preprocessCase ps
        e' <- preprocessExp e
        return $ Case e' ps'
      Let bs e -> error "Let unsupported"
    preprocessCase (pat, body) = do
      let (Pat c vs) = pat
      vs' <- freshVars (length vs)
      let body' = apply (bindAll vs (map Var vs')) body
      return (Pat c vs', body')
    preprocessDef (Definition f vs body) = do
      vs' <- freshVars (length vs)
      body' <- preprocessExp $ apply (bindAll vs (map Var vs')) body
      return $ Definition f vs' body'

preprocess p = evalState (preprocessProgram p) nameGen
  where
    nameGen = initialNameGen "pv" "pf"
