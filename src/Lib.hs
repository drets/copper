{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module Lib () where

import Data.Proxy
import GHC.TypeLits
import GHC.Exts
import Data.Type.Bool
import Data.Kind (Type)

class OrdK (k :: Type) where
  type IsLOE (a :: k) (b :: k) :: Constraint

instance OrdK Nat where
  type IsLOE a b = a <= b

instance OrdK Symbol where
  type IsLOE a b = CmpSymbol a b ~ LT

class OrdK k => Asc (a :: [k]) where

instance (OrdK k) => Asc ('[] :: [k]) where
instance (OrdK k) => Asc ('[n] :: [k]) where
instance (IsLOE x z, Asc (z ': m)) => Asc (x ': z ': m) where

test :: Asc xs => Proxy xs -> ()
test _ = ()

a = test (Proxy :: Proxy '[1])
b = test (Proxy :: Proxy '[1,2,3,4,5])
c = test (Proxy :: Proxy '["a", "b", "c", "d"])
