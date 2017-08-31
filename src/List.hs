{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverlappingInstances #-}
module List where

import Data.Proxy
import Prelude hiding (reverse)

class Append (l1 :: [k]) (l2 :: [k]) l | l1 l2 -> l where
  append :: Proxy l1 -> Proxy l2 -> Proxy l

instance Append  '[] l l where
  append _ _ = Proxy

instance (Append xs ys zs) => Append (x ': xs) ys (x ': zs) where
 append _ _ = Proxy

d = (append (Proxy :: Proxy '[1,2,3]) (Proxy :: Proxy '[4,5,6])) :: Proxy '[1,2,3,4,5,6]

class Reverse (from :: [k]) (to :: [k]) | from -> to where
  reverse :: Proxy from -> Proxy to

instance Reverse '[] '[] where
  reverse _ = Proxy

instance Reverse '[x] '[x] where
  reverse _ = Proxy

instance (Reverse xs m, Append m '[x] zs) => Reverse (x ': xs) zs where
  reverse _ = Proxy

e = (reverse (Proxy :: Proxy '[1,2,3,4,5])) :: Proxy '[5,4,3,2,1]
