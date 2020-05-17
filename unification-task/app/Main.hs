-- Author: Pavlo Tokariev
module Main where

import Unify

main :: IO ()
main = do
  let problem = [(Var (Variable "x"), Var (Variable "z")), (Var (Variable "y"), Func [Var (Variable "x"), Var (Variable "w")])]
  let solution = unification problem
  print problem
  print solution
  
  let typed = TAbs (Variable "x") (Var (Variable "e") )(TAbs (Variable "y") (Var (Variable "t")) (TVar $ Variable "x"))
  let result = typing typed
  print typed
  print result
