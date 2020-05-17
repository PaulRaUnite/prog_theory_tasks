-- Author: Pavlo Tokariev
module Unify where

import qualified Data.Map as Map

newtype Constant = Constant String deriving (Eq, Ord, Show)
newtype Variable = Variable String deriving (Eq, Ord, Show)
data FirstOrderTerm = Const Constant | Var Variable | Func [FirstOrderTerm] deriving (Eq, Ord, Show)

substitution :: (Variable, FirstOrderTerm) -> FirstOrderTerm -> FirstOrderTerm
substitution s c@(Const _) = c
substitution (s, t) (Var v)
  | s == v = t
  | otherwise = Var v
substitution e (Func args) = Func $ map (substitution e) args

vars :: FirstOrderTerm -> [Variable]
vars (Const _) = []
vars (Var v) = [v]
vars (Func args) = foldMap vars args

unification :: [(FirstOrderTerm, FirstOrderTerm)] -> [(FirstOrderTerm, FirstOrderTerm)]
unification list
  | conditionSatisfied = list
  | otherwise = unification $ unificationStep list
  where
    conditionSatisfied = all variableCheck xs
    (xs, sk) = unzip list
    skVars = concatMap vars sk
    variableCheck (Var v) = v `notElem` skVars
    variableCheck _ = False

unificationStep :: [(FirstOrderTerm, FirstOrderTerm)] -> [(FirstOrderTerm, FirstOrderTerm)]
unificationStep ((t1, t2):tail) | t1 == t2 = tail
unificationStep ((f@(Func fargs), g@(Func gargs)):tail)
  | length fargs == length gargs = tail ++ zip fargs gargs
  | otherwise = error "unification of functions which have different amount arguments are not possible"
unificationStep ((f@(Func _), v@(Var _)):tail) = (v, f):unificationStep tail
unificationStep ((Var x, f@(Func _)):_) | x `elem` vars f = error $ "recursion declaration of "++ show x
unificationStep ((Var v, t):tail)
  | notElem v (vars t) && v `elem` allTailVars =
    (map (maptuple2 $ substitution (v, t)) tail) ++ [(Var v, t)]
  where
    allTailVars = concatMap (\(a, b) -> vars a ++ vars b) tail
unificationStep (head:tail) = tail++[head]
unificationStep [] = []

maptuple2 :: (a -> b) -> (a, a) -> (b, b)
maptuple2 f (a1, a2) = (f a1, f a2)

data TypedTerm = TVar Variable | TAbs Variable FirstOrderTerm TypedTerm | TApp TypedTerm TypedTerm deriving (Eq, Show)

newtype TypeGen = TypeGen Int

next :: TypeGen -> TypeGen
next (TypeGen n) = TypeGen $ succ n

var :: TypeGen -> Variable
var (TypeGen n) = Variable $ "a"++show n

problemizeWithContext :: TypedTerm -> Map.Map Variable FirstOrderTerm -> TypeGen -> ([(FirstOrderTerm, FirstOrderTerm)], TypeGen)
problemizeWithContext (TVar v) m tg
  | Map.member v m = ([(Var $ var tg, m Map.! v)], next tg)
  | otherwise = ([], tg)
problemizeWithContext (TAbs v t body) m tg = ((Var $ var tg, Func [t, Var $ var bodyType]) : inner, ntg)
  where
    bodyType = next tg
    (inner, ntg) = problemizeWithContext body (Map.insert v t m) bodyType
problemizeWithContext (TApp t1 t2) m tg = (inner1++inner2, tg2)
    where
      (inner1, tg1) = problemizeWithContext t1 m tg
      (inner2, tg2) = problemizeWithContext t2 m tg1

firstOrderTypingPretty :: FirstOrderTerm -> String
firstOrderTypingPretty (Var (Variable v)) = v
firstOrderTypingPretty (Func args) = "("++ concatMap (\(v, i) -> if length args == i then firstOrderTypingPretty v else firstOrderTypingPretty v ++ " -> ") (zip args [1..]) ++")"
firstOrderTypingPretty (Const (Constant c)) = c

typing :: TypedTerm -> String
typing t = firstOrderTypingPretty result
   where (problem, _) =problemizeWithContext t Map.empty termVariable
         resultList = unification problem
         result =  Map.fromList resultList Map.! (Var $ var termVariable)
         termVariable = TypeGen 1
