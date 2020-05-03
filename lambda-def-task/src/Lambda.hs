-- Author: Pavlo Tokariev, MF-51
module Lambda where

import Prelude

-- 1)
newtype Variable = Variable Int deriving (Eq, Show)

data Term = Var Variable | Func Variable Term | Abs Term Term deriving (Eq)

-- 2)
fresh :: Variable -> Term -> Bool
fresh x (Var y) = x /= y
fresh x (Func y t) = x /= y && fresh x t
fresh x (Abs t' t'') = fresh x t' && fresh x t''

-- 3)
free :: Variable -> Term -> Bool
free x (Var y) = x == y
free x (Func y t) = x/=y && free x t
free x (Abs t' t'') = free x t' || free x t''

-- 4)
subterm :: Term -> Term -> Bool
subterm t' t'' = t' == t'' || case t'' of
  Var v -> False
  Func _ sub -> subterm t' sub
  Abs sub' sub'' -> subterm t' sub' || subterm t' sub''

-- 5)
bound :: Variable -> Term -> Bool
bound x (Var _) = False
bound x (Func y t) = x == y || bound x t
bound x (Abs t' t'') = bound x t' || bound x t''
