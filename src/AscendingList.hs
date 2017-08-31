{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module AscendingList () where

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
