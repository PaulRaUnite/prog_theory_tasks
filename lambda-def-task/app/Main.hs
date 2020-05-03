-- Author: Pavlo Tokariev, MF-51
module Main where

import Lambda

main :: IO ()
main = do
  let x = Variable 1
  let y = Variable 2
  let z = Variable 3
  let u = Variable 4
  let xy = Abs (Var x) (Var y)
  let yz = Abs (Var y) (Var z)
  let example1 = Func x (Func y (Abs xy (Var z)))

  putStrLn "is u fresh in \\x.\\y.xyz?"
  print $ fresh u example1
  putStrLn "is x fresh in \\x.\\y.xyz?"
  print $ fresh x example1
  putStrLn "is y fresh in \\x.\\y.xyz?"
  print $ fresh y example1
  putStrLn "is z fresh in \\x.\\y.xyz?"
  print $ fresh z example1

  putStrLn "---------------------"
  putStrLn "is z free in \\x.\\y.xyz?"
  print $ free z example1
  putStrLn "is x free in \\x.\\y.xyz?"
  print $ free x example1
  putStrLn "is y free in \\x.\\y.xyz?"
  print $ free y example1

  putStrLn "---------------------"
  putStrLn "is xy subterm of \\x.\\y.xyz?"
  print $ subterm xy example1
  putStrLn "is yz subterm of \\x.\\y.xyz?"
  print $ subterm yz example1

  putStrLn "---------------------"
  putStrLn "is y bound in \\x.\\y.xyz?"
  print $ bound y example1
  putStrLn "is x bound in \\x.\\y.xyz?"
  print $ bound x example1
  putStrLn "is z bound in \\x.\\y.xyz?"
  print $ bound z example1
