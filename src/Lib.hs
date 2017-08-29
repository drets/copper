{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
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

type family Asc (a :: [k]) :: Constraint where
  Asc '[]     = ()
  Asc '[n]    = ()
  Asc (x:z:m) = (IsLOE x z, Asc (z:m))

test :: Asc xs => Proxy xs -> ()
test _ = ()

a = test (Proxy :: Proxy '[1])
b = test (Proxy :: Proxy '[1,2,3,4,5])
c = test (Proxy :: Proxy '["a", "b", "c", "d"])
