module Tasks where

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

taskSum = mkProgram $ do
  define "sum" ["s", "x"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ var "x"
      of_ "Cons" ["y", "ys"] $ app "sum" [var "ys", app "+" [var "x", var "y"]]
  define "upto" ["m", "n"] $
    case_ (app ">" [var "m", var "n"]) $ do
      of_ "True" [] $ cons0 "Nil"
      of_ "False" [] $ cons "Cons" [var "m", app "upto" [app "+" [var "m", one], var "n"]]
  return $
    app "sum" [app "upto" [one, var "z"], zero]
    
taskUpto = mkProgram $ do
  define "upto" ["m", "n"] $
    case_ (app ">" [var "m", var "n"]) $ do
      of_ "True" [] $ cons0 "Nil"
      of_ "False" [] $ cons "Cons" [var "m", app "upto" [app "+" [var "m", one], var "n"]]
  return $
    app "upto" [one, var "z"]

taskSumDouble = mkProgram $ do
  define "sum" ["s", "x"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ var "x"
      of_ "Cons" ["y", "ys"] $ app "sum" [var "ys", app "+" [var "x", var "y"]]
  define "upto" ["m", "n"] $
    case_ (app ">" [var "m", var "n"]) $ do
      of_ "True" [] $ cons0 "Nil"
      of_ "False" [] $ cons "Cons" [var "m", app "upto" [app "+" [var "m", one], var "n"]]
  define "double" ["s"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ cons0 "Nil"
      of_ "Cons" ["x", "xs"] $ cons "Cons" [app "+" [var "x", var "x"], app "double" [var "xs"]]
  return $
    app "sum" [app "double" [app "upto" [one, var "z"]], zero]

taskSumSquares = mkProgram $ do
  define "sum" ["s", "x"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ var "x"
      of_ "Cons" ["y", "ys"] $ app "sum" [var "ys", app "+" [var "x", var "y"]]
  define "upto" ["m", "n"] $
    case_ (app ">" [var "m", var "n"]) $ do
      of_ "True" [] $ cons0 "Nil"
      of_ "False" [] $ cons "Cons" [var "m", app "upto" [app "+" [var "m", one], var "n"]]
  define "squares" ["s"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ cons0 "Nil"
      of_ "Cons" ["x", "xs"] $ cons "Cons" [app "*" [var "x", var "x"], app "squares" [var "xs"]]
  return $
    app "sum" [app "squares" [app "upto" [one, var "z"]], zero]

taskMultAB = mkProgram $ do
  define "mult" ["x", "y"] $ 
    case_ (var "x") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["v"] $ cons "S" [app "add" [var "v", var "y"]]
  define "add" ["x", "y"] $ 
    case_ (var "x") $ do
      of_ "Z" [] $ cons0 "Z"
      of_ "S" ["v"] $ app "add" [app "mult" [var "v", var "y"], var "y"]
  return $
    app "mult" [var "a", var "b"]

taskRemoveTrans = mkProgram $ do
  define "f1" ["x"] $ app "f2" [var "x"]
  define "f2" ["x"] $ app "f1" [var "x"]
  return $ app "f1" [var "a"]

taskGlobalVsLocal = mkProgram $ do
  define "g" ["x"] $ 
    case_ (var "x") $ do
      of_ "True" [] $ cons0 "True"
      of_ "False" [] $ app "g" [cons0 "False"]
  return $ app "g" [var "x"]

taskFromGeneral = mkProgram $ do
  define "f" ["x"] $ app "f" [cons "S" [var "x"]]
  return $ 
    app "f" [var "a"]

-- Does not work
taskFromEmb = mkProgram $ do
  define "f" ["x"] $ app "g" [app "f" [var "x"]]
  define "g" ["x"] $ 
    case_ (var "x") $ do
      of_ "A" [] $ cons0 "B"
  return $ 
    app "f" [var "a"]

taskExpToLiear = mkProgram $ do
  define "g" ["x", "y"] $
    case_ (var "x") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["z"] $ app "g" [app "g" [var "z", var "z"], app "g" [var "z", var "z"]]
  return $
    app "g" [var "a", var "a"]

taskEvenDouble = mkProgram $ do
  define "even" ["x"] $
    case_ (var "x") $ do
      of_ "Z" [] $ cons0 "True"
      of_ "S" ["y"] $ app "odd" [var "y"]
  define "odd" ["x"] $
    case_ (var "x") $ do
      of_ "Z" [] $ cons0 "False"
      of_ "S" ["y"] $ app "even" [var "y"]
  define "double" ["x"] $
    case_ (var "x") $ do
      of_ "Z" [] $ cons0 "Z"
      of_ "S" ["y"] $ cons "S" [cons "S" [app "double" [var "y"]]]
  return $
    app "even" [cons "S" [app "double" [var "a"]]]

taskEraseOmega = mkProgram $ do
  define "omega" ["x"] $ app "omega" [var "x"]
  define "erase" ["x"] $ cons0 "Stop"
  return $
    app "erase" [app "omega" [var "a"]]
  
taskEqSame = mkProgram $ do
  define "eq" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ app "eqZ" [var "y"]
      of_ "S" ["x"] $ app "eqS" [var "y", var "x"]
  define "eqZ" ["s"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "True"
      of_ "S" ["x"] $ cons0 "False"
  define "eqS" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "False"
      of_ "S" ["x"] $ app "eq" [var "x", var "y"]
  return $
    app "eq" [var "a", var "a"]

taskEqTwo = mkProgram $ do
  define "eq" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ app "eqZ" [var "y"]
      of_ "S" ["x"] $ app "eqS" [var "y", var "x"]
  define "eqZ" ["s"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "True"
      of_ "S" ["x"] $ cons0 "False"
  define "eqS" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "False"
      of_ "S" ["x"] $ app "eq" [var "x", var "y"]
  return $
    app "eq" [two, var "a"]

-- Not working properly
taskCompareLength = mkProgram $ do
  define "eq" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ app "eqZ" [var "y"]
      of_ "S" ["x"] $ app "eqS" [var "y", var "x"]
  define "eqZ" ["s"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "True"
      of_ "S" ["x"] $ cons0 "False"
  define "eqS" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons0 "False"
      of_ "S" ["x"] $ app "eq" [var "x", var "y"]
  define "length" ["s"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ cons0 "Z"
      of_ "Cons" ["x", "xs"] $ cons "S" [app "length" [var "xs"]]
  return $
    app "eq" [app "length" [var "as"], app "length" [var "bs"]]

taskDoubleAppend = mkProgram $ do
  define "append" ["s", "vs"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ var "vs"
      of_ "Cons" ["u", "us"] $ cons "Cons" [var "u", app "append" [var "us", var "vs"]]
  return $
    app "append" [app "append" [var "xs", var "ys"], var "zs"]

taskTripleAppend = mkProgram $ do
  define "append" ["s", "vs"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ var "vs"
      of_ "Cons" ["u", "us"] $ cons "Cons" [var "u", app "append" [var "us", var "vs"]]
  return $
    app "append" [app "append" [app "append" [var "xs", var "ys"], var "zs"], var "as"]

taskAckermann = mkProgram $ do
  define "a" ["s", "n"] $
    case_ (var "s") $ do
      of_ "Z" [] $ cons "S" [var "n"]
      of_ "S" ["m"] $ app "b" [var "n", var "m"]
  define "b" ["s", "m"] $
    case_ (var "s") $ do
      of_ "Z" [] $ app "a" [var "m", one]
      of_ "S" ["n"] $ app "a" [var "m", app "a" [cons "S" [var "m"], var "n"]]
  define "ack" ["x", "y"] $ app "a" [var "x", var "y"]
  return $
    app "ack" [var "a", var "b"]

taskAddAcc_SSa_b = mkProgram $ do
  define "addAcc" ["s", "y"] $ 
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ app "addAcc" [var "x", cons "S" [var "y"]]
  return $
    app "addAcc" [cons "S" [cons "S" [var "a"]], var "b"]

taskAddAcc_a_Z = mkProgram $ do
  define "addAcc" ["s", "y"] $ 
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ app "addAcc" [var "x", cons "S" [var "y"]]
  return $
    app "addAcc" [var "a", zero]

taskAddAcc_a_b = mkProgram $ do
  define "addAcc" ["s", "y"] $ 
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ app "addAcc" [var "x", cons "S" [var "y"]]
  return $
    app "addAcc" [var "a", var "b"]

taskAdd_a_a = mkProgram $ do
  define "add" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ cons "S" [app "add" [var "x", var "y"]]
  return $
    app "add" [var "a", var "a"]

taskAdd_a_b = mkProgram $ do
  define "add" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ cons "S" [app "add" [var "x", var "y"]]
  return $
    app "add" [var "a", var "b"]
    
taskAddAdd_a_b_c = mkProgram $ do
  define "add" ["s", "y"] $
    case_ (var "s") $ do
      of_ "Z" [] $ var "y"
      of_ "S" ["x"] $ cons "S" [app "add" [var "x", var "y"]]
  return $
    app "add" [app "add" [var "a", var "b"], var "c"]
  
defineKMPUtils = do
  define "eqA" ["eqA_arg"] $
    case_ (var "eqA_arg") $ do
      of_ "A" [] $ cons0 "True"
      of_ "B" [] $ cons0 "False"
      of_ "C" [] $ cons0 "False"
  define "eqB" ["eqB_arg"] $
    case_ (var "eqB_arg") $ do
      of_ "A" [] $ cons0 "False"
      of_ "B" [] $ cons0 "True"
      of_ "C" [] $ cons0 "False"
  define "eqC" ["eqC_arg"] $
    case_ (var "eqC_arg") $ do
      of_ "A" [] $ cons0 "False"
      of_ "B" [] $ cons0 "False"
      of_ "C" [] $ cons0 "True"
  define "eqSymb" ["eqSymb_s", "eqSymb_y"] $
    case_ (var "eqSymb_s") $ do
      of_ "A" [] $ app "eqA" [var "eqSymb_y"]
      of_ "B" [] $ app "eqB" [var "eqSymb_y"]
      of_ "C" [] $ app "eqC" [var "eqSymb_y"]
  define "if" ["s", "x", "y"] $
    case_ (var "s") $ do
      of_ "True" [] $ var "x"
      of_ "False" [] $ var "y"
  define "match" ["p", "s"] $ app "m" [var "p", var "s", var "p", var "s"]
  define "m" ["s", "ss", "op", "os"] $
    case_ (var "s") $ do
      of_ "Nil" [] $ true
      of_ "Cons" ["p", "pp"] $ app "mx" [var "ss", var "p", var "pp", var "op", var "os"]
  define "mx" ["sc", "p", "pp", "op", "os"] $
    case_ (var "sc") $ do
      of_ "Nil" [] $ false
      of_ "Cons" ["s", "ss"] $ app "if" [app "eqSymb" [var "p", var "s"], app "m" [var "pp", var "ss", var "op", var "os"], app "mn" [var "os", var "op"]]
  define "mn" ["sc", "op"] $ 
    case_ (var "sc") $ do
      of_ "Nil" [] $ false
      of_ "Cons" ["s", "ss"] $ app "m" [var "op", var "ss", var "op", var "ss"]


taskKMP_A = mkProgram $ do
  defineKMPUtils
  return $
    app "match" [cons "Cons" [cons0 "A", cons0 "Nil"], var "str"]

taskKMP_AA = mkProgram $ do
  defineKMPUtils
  return $
    app "match" [cons "Cons" [cons0 "A", cons "Cons" [cons0 "A", cons0 "Nil"]], var "str"]

taskKMP_ABA = mkProgram $ do
  defineKMPUtils
  return $
    app "match" [cons "Cons" [cons0 "A", cons "Cons" [cons0 "B", cons "Cons" [cons0 "A", cons0 "Nil"]]], var "str"]

taskKMP_ABACABA = mkProgram $ do
  defineKMPUtils
  return $
    app "match" [makeStr "ABACABA", var "str"]



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