module Main where

import Lib

import Building
import Lang

example = mkProgram $ do
  define "not" ["x"] $
    case_ (var "x") $ do
      of_ "True" [] $ cons0 "False"
      of_ "False" [] $ cons0 "True"
  define "and" ["x", "y"] $
    case_ (var "x") $ do
      of_ "True" [] $
        case_ (var "y") $ do
          of_ "True" [] $ cons0 "True"
          of_ "False" [] $ cons0 "False"
      of_ "False" [] $ cons0 "False"
  return $
    app "and" [app "not" [var "x"], app "not" [cons0 "False"]]

sumRange = mkProgram $ do
  define "sum" ["xs", "a"] $
    case_ (var "xs") $ do
      of_ "Nil" [] $ var "a"
      of_ "Con" ["y", "ys"] $ app "sum" [var "ys", app "+" [var "a", var "y"]]
  define "upto" ["m", "n"] $
    case_ (app ">" [var "m", var "n"]) $ do
      of_ "True" [] $ cons0 "Nil"
      of_ "False" [] $ cons "Con" [var "m", app "upto" [app "+" [var "m", one], var "n"]]
  return $
    app "sum" [app "upto" [one, var "z"], zero]


zero = cons0 "Z"
one = cons "S" [zero]
two = cons "S" [one]
three = cons "S" [two]

taskCompose = mkProgram $ do
  define "a" ["ax"] $
    case_ (var "ax") $ do
      of_ "Stop" [] $ cons0 "Stop"
      of_ "A" ["x1"] $ cons "B" [app "a" [var "x1"]]
  define "b" ["bx"] $ do
    case_ (var "bx") $ do
      of_ "Stop" [] $ cons0 "Stop"
      of_ "B" ["x2"] $ cons "C" [app "b" [var "x2"]]
  return $
    app "b" [app "a" [var "u"]]



main :: IO ()
main = someFunc
