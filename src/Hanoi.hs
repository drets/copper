{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hanoi where

import Prelude hiding (concat)

import Data.Kind (Type)
import GHC.TypeLits
import GHC.Exts
import Data.Proxy

type family Append a b where
  Append '[] l        = l
  Append (x ': xs) ys = x ': (Append xs ys)

type family Hanoi a x y z where
  Hanoi 0 _ _ _ = '[]
  Hanoi n a b c = Append (Append (Hanoi (n-1) a c b) '[ '(a,b) ]) (Hanoi (n-1) c b a)

class Reflect (xs :: [(Symbol, Symbol)]) where
 reflect :: Proxy xs -> [(String, String)]

instance Reflect '[] where
  reflect _ = []

instance (KnownSymbol a, KnownSymbol b, Reflect xs) => Reflect ('(a, b) ': xs) where
  reflect _ = (symbolVal (Proxy :: Proxy a), symbolVal (Proxy :: Proxy b)) : reflect (Proxy :: Proxy xs)

hanoi :: (result ~ Hanoi 3 "a" "b" "c") => Proxy result
hanoi = Proxy

result = reflect hanoi

{-

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b  ++ [(a, b)] ++ hanoi (n-1) c b a

> hanoi 2 "a" "b" "c"
[("a","c"),("a","b"),("c","b")]
> hanoi 3 "a" "b" "c"
[("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]

-}
