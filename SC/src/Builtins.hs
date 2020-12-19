module Builtins where

import Lang
import Data.List
import Data.Maybe

import Substitution
import Building

import Debug.Trace

evalBuiltin b args = case b of
    BuiltinName "+" -> evalPlus args
    BuiltinName ">" -> evalGreater args

unfoldBuiltin b args = case b of
    BuiltinName "+" -> Nothing --unfoldPlus args
    BuiltinName ">" -> Nothing --unfoldGreater args

fromChurch (Cons (ConsName "Z") []) = Just 0
fromChurch (Cons (ConsName "S") [next]) = (1 +) <$> fromChurch next
fromChurch e = Nothing

toChurch 0 = Cons (ConsName "Z") []
toChurch n
    | n == 0 = Cons (ConsName "Z") []
    | n > 0 = Cons (ConsName "S") [toChurch (n - 1)]
    | n < 0 = error $ "toChurch on " ++ show n

unfoldPlus [a, b] = do 
    --traceM $ show a ++  " + " ++ show b
    ra <- fromChurch a
    rb <- fromChurch b
    return $ toChurch (ra + rb)
unfoldPlus args = Nothing

unfoldGreater [a, b] = do
    ra <- fromChurch a
    rb <- fromChurch b
    if ra > rb
        then return $ cons0 "True"
        else return $ cons0 "False"
unfoldGreater args = Nothing

evalPlus [a, b] = fromMaybe (error $ "+ on " ++ show [a, b]) $ do
    ra <- fromChurch a
    rb <- fromChurch b
    return $ toChurch (ra + rb)
evalPlus args = error $ "+ on " ++ show args

evalGreater [a, b] = fromMaybe (error $ "> on " ++ show [a, b]) $ do
  --traceM $ show a ++  " > " ++ show b
  ra <- fromChurch a
  rb <- fromChurch b
  if ra > rb
    then return $ cons0 "True"
    else return $ cons0 "False"
evalGreater args = error $ "+ on " ++ show args

unfold defs f args = case find (\(Definition f' _ _) -> f == f') defs of
  Nothing -> error $ "Can't unfold. No such function " ++ show f
  Just (Definition f' vs e) ->
    let subst = bindAll vs args
        e' = apply subst e
     in e'

matchPattern patterns cons@(Cons c es) =
  match patterns
  where
    match [] = error $ "Can't match " ++ show (Cons c es) ++ " in " ++ show patterns
    match ((Pat pc vs, body) : rest) = if c == pc && length vs == length es then (Pat pc vs, body) else match rest
