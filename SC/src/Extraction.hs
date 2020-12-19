{-# LANGUAGE FlexibleContexts #-}

module Extraction where

--import Debug.Trace

import Builtins
import Control.Monad.State
import Decomp
import Lang
import NameGen
import ProcessTree
import Substitution

data Signatures = Signatures [(Node, Exp)]

addSignature (Signatures ss) node exp = Signatures $ (node, exp) : ss

getSignature (Signatures ss) node = lookup node ss

emptySignatures = Signatures []

extract sigs node = do
  e <- getExpression node
  extractExpr e
  where
    extractExpr (Let vs _) = do
      children_es <- getChildren node
      (e0, def0) : rest <- mapM (extract sigs) children_es
      let (es, defs) = unzip rest
      let vars = map fst vs
      return (Let (zip vars es) e0, def0 ++ concat defs)
    extractExpr expr = case decompose expr of
      Observable (Var v) ->
        return (Var v, [])
      Observable (Cons c _) -> do
        children_es <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip children_es
        return (Cons c es, concat defs)
      Observable (Call (Undef f) _) -> do
        children_es <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip children_es
        return (Call (Undef f) es, concat defs)
      Observable (Call (Builtin b) _) -> do
        children_es <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip children_es
        return (Call (Builtin b) es, concat defs)
      WithContext (Case (Cons c _) _) ctx -> do
        extract sigs . head =<< getChildren node
      WithContext (Call (Func f) args) ctx -> do
        let mkFunc fds = do
              let (_, dom) = unzip . snd . head $ fds
              f' <- lift freshFunc
              vs <- lift $ freshVars (length dom)
              let subst = bindAll dom (map Var vs)
              --traceM $ show dom
              let sigs' = addSignature sigs node (Call (Func f') (map Var dom))
              [(e, defs)] <- mapM (extract sigs') =<< getChildren node
              let def = Definition f' vs (apply subst e)
              --let def = Definition f' vs e
              return (Call (Func f') (map Var dom), def : defs)
        let mkFold base ren = do
              let Just sig = getSignature sigs base
              --traceM $ show sig
              return (apply (fromRenaming $ invRenaming ren) sig, [])
        let mkOtherwise = do
              extract sigs . head =<< getChildren node
        fds <- getFoldings node
        fto <- getFoldingTo node
        case (fds, fto) of
          ([], Nothing) -> mkOtherwise
          ([], Just (base, ren)) -> mkFold base ren
          (_, _) -> mkFunc fds
      WithContext (Call (Builtin b) args) ctx -> do
        case unfoldBuiltin b args of
          Just e -> extract sigs . head =<< getChildren node
          Nothing -> do
            children_es <- mapM (extract sigs) =<< getChildren node
            let (es, defs) = unzip children_es
            return (Call (Builtin b) es, concat defs)
      WithContext (Case (Var v) ps) ctx -> do
        (e0, def0) : rest <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip rest
        let pats = map fst ps
        let newps = zip pats es
        return (Case e0 newps, def0 ++ concat defs)
      WithContext (Case (Call (Undef f) args) ps) ctx -> do
        (e0, def0) : rest <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip rest
        let pats = map fst ps
        let newps = zip pats es
        return (Case e0 newps, def0 ++ concat defs)
      WithContext (Case (Call (Builtin b) args) ps) ctx -> do
        (e0, def0) : rest <- mapM (extract sigs) =<< getChildren node
        let (es, defs) = unzip rest
        let pats = map fst ps
        let newps = zip pats es
        return (Case e0 newps, def0 ++ concat defs)
      x -> error $ show x

extractFromProcessTree processTree = do
  (e, defs) <- evalStateT (evalStateT (extract emptySignatures 0) processTree) (nameGen "ev" "ef")
  return $ Program e defs
  where
    nameGen = initialNameGen
