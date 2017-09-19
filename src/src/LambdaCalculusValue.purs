module LambdaCalculusValue where

import Data.String.Utils (replaceAll)
import Prelude (class Show, show, (<>))

data Lambda = Var String | App Lambda Lambda | Abs String Lambda

instance showLambda :: Show Lambda where
  show (Var s)   = show s
  show (App s m) = "(" <> show s <> ") " <> show m
  show (Abs s m) = "λ" <> s <> " -> " <> show m

eval :: Lambda -> Lambda
eval (Var s)                          = Var s
eval (Abs s m)                        = Abs s (eval m)
eval (App (Abs x (Var t12)) (Var v2)) = Var (replaceAll x v2 t12)
eval (App v@(Abs x (Var _)) t2)       = eval (App v (eval t2))
eval (App t1 t2)                      = eval (App (eval t1) t2)

a = eval (App (Abs "x" (Var "x")) (Var "y"))
b = eval (Abs "x" (Var "x"))
t = eval (App (Abs "k" (Var "xk")) (Var "m"))
c = eval (App (Abs "k" (Var "xk")) (App (Abs "x" (Var "xz")) (Var "y")))
