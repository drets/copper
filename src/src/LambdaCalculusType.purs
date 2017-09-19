module LambdaCalculusType where

foreign import kind SList
foreign import data SNil :: SList
foreign import data SCons :: Symbol -> SList -> SList

foreign import kind Expr
foreign import data Var :: SList -> Expr
foreign import data App :: Expr -> Expr -> Expr
foreign import data Abs :: Symbol -> Expr -> Expr

data LambdaProxy (l :: Expr) = LambdaProxy

class Calc (i :: Expr) (o :: Expr) | i -> o

instance var :: Calc (Var s) (Var s)
instance abs :: Calc m m' => Calc (Abs s m) (Abs s m')
instance aappRepl :: ReplaceAll x v2 t12 r => Calc (App (Abs x (Var t12)) (Var v2)) (Var r)
instance bappRight ::
  ( Calc t2 t2'
  , Calc (App (Abs x (Var unused)) t2') m'
  ) => Calc (App (Abs x (Var unused)) t2) m'
instance cappLeft ::
  ( Calc t1 t1'
  , Calc (App t1' t2) m'
  ) => Calc (App t1 t2) m'

class ReplaceAll (x :: Symbol) (v2 :: SList) (t12 :: SList) (r :: SList) | x v2 t12 -> r

class Append (l1 :: SList) (l2 :: SList) (l :: SList) | l1 l2 -> l

instance appendNil :: Append SNil l l
instance appendConst ::
  (Append xs ys zs) => Append (SCons x xs) ys (SCons x zs)

instance replaceAllNil :: ReplaceAll x v2 SNil SNil

instance _replaceAllConsS ::
  ( Append ys xs zs
  , ReplaceAll x (SCons y ys) zs res
  ) => ReplaceAll x (SCons y ys) (SCons x xs) (SCons y res)

instance replaceAllConsF ::
  ( ReplaceAll x y xs res
  ) => ReplaceAll x y (SCons w xs) (SCons w res)

parse :: forall xs ys. Calc xs ys => LambdaProxy xs -> LambdaProxy ys
parse _ = LambdaProxy

res1 = parse (LambdaProxy :: LambdaProxy (Var (SCons "x" SNil)))
res2 = parse (LambdaProxy :: LambdaProxy (Abs "x" (Var (SCons "x" SNil))))
res3 = parse (LambdaProxy :: LambdaProxy (App (Abs "x" (Var (SCons "x" SNil))) (Var (SCons "y" SNil))))
res4 = parse (LambdaProxy :: LambdaProxy (App (Abs "x" (Var (SCons "x" (SCons "y" (SCons "x" SNil))))) (Var (SCons "z" SNil))))
res5 = parse (LambdaProxy :: LambdaProxy (App (Abs "x" (Var (SCons "x" SNil))) (Var (SCons "y" (SCons "z" SNil)))))
res6 = parse (LambdaProxy :: LambdaProxy (App (Abs "k" (Var (SCons "x" (SCons "k" SNil)))) (App (Abs "x" (Var (SCons "x" (SCons "z" SNil)))) (Var (SCons "y" SNil)))))
