{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverlappingInstances #-}
module Lib () where

import Data.Proxy
import GHC.TypeLits
import GHC.Exts
import Data.Type.Bool
import Data.Kind (Type)
import Prelude hiding (reverse)

class OrdK (k :: Type) where
  type IsLOE (a :: k) (b :: k) :: Constraint

instance OrdK Nat where
  type IsLOE a b = a <= b

instance OrdK Symbol where
  type IsLOE a b = CmpSymbol a b ~ LT

class Asc (list :: [k]) bool | list -> bool

instance Asc '[] True
instance Asc '[n] True
instance (IsLOE x z, Asc (z ': m) True) => Asc (x ': z ': m) True

test :: Asc xs True => Proxy xs -> ()
test _ = ()

a = test (Proxy :: Proxy '[1])
b = test (Proxy :: Proxy '[1,2,3,4,5])
c = test (Proxy :: Proxy '["a", "b", "c", "d"])

--Concat two type level lists

class ListConcat (l1 :: [k]) (l2 :: [k]) l | l1 l2 -> l where
  concatList :: Proxy l1 -> Proxy l2 -> Proxy l

instance ListConcat  '[] l l where
  concatList _ _ = Proxy

instance (ListConcat xs ys zs) => ListConcat (x ': xs) ys (x ': zs) where
 concatList _ _ = Proxy

d = (concatList (Proxy :: Proxy '[1,2,3]) (Proxy :: Proxy '[4,5,6])) :: Proxy '[1,2,3,4,5,6]

class ListReverse (from :: [k]) (to :: [k]) | from -> to where
  reverse :: Proxy from -> Proxy to

instance ListReverse '[] '[] where
  reverse _ = Proxy

instance ListReverse '[x] '[x] where
  reverse _ = Proxy

instance (ListReverse xs m, ListConcat m '[x] zs) => ListReverse (x ': xs) zs where
  reverse _ = Proxy

e = (reverse (Proxy :: Proxy '[1,2,3,4,5])) :: Proxy '[5,4,3,2,1]
