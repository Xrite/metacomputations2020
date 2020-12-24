module Main where

import Lib
import Data.Text.Prettyprint.Doc (pretty)
import Supercompilation
import Tasks
import Substitution
import Lang
import Eval
import Building

main :: IO ()
main = do
  show . pretty <$> supercompile taskSumSquares >>= putStrLn
  putStrLn "---------------"
  show . pretty <$> supercompile taskKMP_ABACABA >>= putStrLn
  let s = single (VarName "str") (makeStr "AAABCABACABAB")
  putStrLn "result on AAABCABACABAB"
  show . pretty . evalWithVars s <$> supercompile taskKMP_ABACABA >>= putStrLn


