{-# LANGUAGE FlexibleContexts #-}

module Deforestration where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
--import Text.Pretty.Simple
--import Debug.Pretty.Simple
--import Debug.Trace
import Decomp
import Driving
import Embedding
import Generalization
import Lang
import NameGen
import ProcessTree
import Substitution

deforest :: (MonadTrans t, MonadState ProcessTree (t m), MonadState NameGen m) => t m ()
deforest = do
  maybeNode <- popUnprocessedLeaf
  --traceM $ show maybeNode
  case maybeNode of
    Nothing -> return ()
    Just node -> processNode node >> deforest

processNode node = do
  foldSuccessful <- tryFold node
  --when foldSuccessful (error "aaa")
  --traceM $ show foldSuccessful
  if foldSuccessful
    then return ()
    else do
      generalizationSuccessful <- tryGeneralize node
      if generalizationSuccessful
        then return ()
        else drive node

tryFold n = do
  expr <- getExpression n
  case expr of
    Call (Func f) args -> foldCall expr
    _ -> return False
  where
    foldCall expr = do
      ancestors <- getAncestors n
      ancestor_es <- mapM getExpression ancestors
      case find (isRenaming expr . snd) (zip ancestors ancestor_es) of
        Nothing -> return False
        Just (t, e) -> do
          let Just r = renaming expr e
          foldTo n t r
          return True
    ren expr t = do
      e <- getExpression t
      case renaming expr e of
        Just subst -> return $ Just (t, subst)
        Nothing -> return Nothing

tryGeneralize node = do
  expr <- getExpression node
  case expr of
    Call (Func f) args -> generalize expr
    _ -> return False
  where
    generalize expr = do
      ancestors <- getAncestors node
      ancestor_es <- mapM getExpression ancestors
      case find ((`byCoupling` expr) . snd) (zip ancestors ancestor_es) of
        Nothing -> return False
        Just (t, e) -> do
          --error "found"
          --traceM $ "Node " ++ show t
          doGeneralization t
          return True
    doGeneralization t = do
      e <- getExpression node
      e' <- getExpression t
      Generalization eg subst1 subst2 <- lift $ mostSpecificGeneralization e e'
      --pTraceShowM $ (Generalization eg subst1 subst2)
      case renaming eg e' of
        Just _ -> do
          abstract node eg subst1
        Nothing -> do
          --traceM $ "eg " ++ show eg
          --traceM $ "e' " ++ show eg
          abstract t eg subst2

    abstract t eg subst = do
      let newExpr = Let (bindings subst) eg
      replaceNodeWith t newExpr

buildProcessTree program =
  let state = initialProcessTree program
      nameGen = initialNameGen
   in evalState (execStateT deforest state) (nameGen "dv" "df")
