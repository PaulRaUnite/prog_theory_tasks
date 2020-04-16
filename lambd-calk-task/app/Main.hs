-- Author: Pavlo Tokariev, MF-51
module Main where

import Lambda

-- substitution substitutes term into another term instead of specified variable
substitution :: Variable -> Term -> Term -> Term
substitution x n (VTm y)
  | x == y = n
  | otherwise = VTm y
substitution x n f@(FTm y m)
  | x == y = f
  | not (x `elem` fvars m) = f
  | not (y `elem` fvars n) = FTm y $ substitution x n m
  | otherwise = let z = head [Var v | v <- [0..], free (Var v) m, free (Var v) n]
                in FTm z $ substitution x n $ substitution y (VTm z) m
substitution x n (ATm m' m'') = ATm (substitution x n m') (substitution x n m'')

-- eta_reduction eliminate abstraction if inner term do not use bounded variable
eta_reduction :: Term -> Term
eta_reduction t@(FTm x (ATm f (VTm v)))
  | x == v && not (x `elem` fvars f) = f
  | otherwise = t
eta_reduction t = t

-- beta_reduction is substitution for application with abstraction first
beta_reduction :: Term -> Term
beta_reduction (ATm (FTm x m) n) = substitution x n m
beta_reduction t = t

-- normal_reduction deduces leftmost outermost redexes first
normal_reduction :: Term -> Term
normal_reduction t@(VTm _) = t
normal_reduction t@(FTm x body)
  | eta /= t = normal_reduction eta
  | otherwise = FTm x $ normal_reduction body
 where eta = eta_reduction t
normal_reduction t@(ATm m' m'')
  | beta /= t = normal_reduction beta
  | otherwise = ATm (normal_reduction m') (normal_reduction m'')
  where beta = beta_reduction t

main :: IO ()
main = do
    let x = Var 1
    let y = Var 2
    let m = FTm x $ ATm (VTm x) (VTm y)
    let n1 = VTm x
    putStr $ show m ++ "[" ++ show x ++ ":=" ++ show n1 ++ "] -> "
    putStrLn $ show $ substitution x n1 m
    putStr $ show m ++ "[" ++ show y ++ ":=" ++ show n1 ++ "] -> "
    putStrLn $ show $ substitution y n1 m
    putStr $ show (ATm (VTm x) (VTm y)) ++ "[" ++ show x ++ ":=" ++ show m ++ "] -> "
    putStrLn $ show $ substitution x m $ ATm (VTm x) (VTm y)

    let z = Var 3
    let e = ATm (FTm x (ATm (VTm x) (VTm y))) (VTm z)
    putStr $ (show e) ++ " ->>normal reduction->> "
    putStrLn $ show $ normal_reduction e
