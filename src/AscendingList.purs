module AscendingList where

import Type.Data.Ordering (LT)
import Type.Data.Symbol (class CompareSymbol)

foreign import kind SList
foreign import data SNil :: SList
foreign import data SCons :: Symbol -> SList -> SList

data SListProxy (l :: SList) = SListProxy

class Sort (i :: SList) (o :: SList) | i -> o

instance sortSNil  :: Sort SNil SNil
instance sortSOne  :: Sort (SCons m SNil) SNil
instance sortSCons :: (CompareSymbol x y LT, Sort (SCons y ys) SNil) => Sort (SCons x (SCons y ys)) SNil

test :: forall xs ys. Sort xs ys => SListProxy xs -> SListProxy ys
test _ = SListProxy

ascendingList = test (SListProxy :: SListProxy (SCons "a" (SCons "b" (SCons "c" SNil))))
-- compilationError = test (SListProxy :: SListProxy (SCons "b" (SCons "a" (SCons "c" SNil))))
