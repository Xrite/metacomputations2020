{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module NameGen where

import Control.Lens hiding (children, folding, (:<))
import Control.Monad.State
import Lang

data NameGen = NameGen
  { _varsCnt :: Int,
    _funcCnt :: Int,
    _varPref :: String,
    _funcPref :: String
  }
  deriving (Show, Eq)

makeLenses ''NameGen

freshVar :: MonadState NameGen m => m Variable
freshVar = do
  num <- use varsCnt
  varsCnt += 1
  vp <- use varPref
  return (VarName $ vp ++ show num)

freshVars :: MonadState NameGen m => Int -> m [Variable]
freshVars cnt = do
  num <- use varsCnt
  varsCnt += cnt
  vp <- use varPref
  let vars = map (\n -> VarName $ vp ++ show n) [num .. num + cnt - 1]
  return vars

freshFunc :: MonadState NameGen m => m Function
freshFunc = do
  num <- use funcCnt
  funcCnt += 1
  fp <- use funcPref
  return (FuncName $ fp ++ show num)

initialNameGen :: String -> String -> NameGen
initialNameGen vp fp =
  NameGen
    { _varsCnt = 0,
      _funcCnt = 0,
      _varPref = vp,
      _funcPref = fp
    }
