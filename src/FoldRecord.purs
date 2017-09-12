module FoldRecord (main) where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Record (delete, get)
import Global.Unsafe (unsafeStringify)
import Prelude (Unit, discard, ($), (*), (+), (<<<), (<>))
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, SProxy(SProxy))
import Type.Row (Cons, Nil, RLProxy(..), kind RowList)


fold :: forall xs a lxs.
       RowToList xs lxs
    => FoldRecord lxs xs a
    => (a -> a -> a) -> Record xs -> Maybe a
fold = foldRecord (RLProxy :: RLProxy lxs)

class FoldRecord
      (ra :: RowList)
      (rt :: # Type)
      (a  :: Type)
      | rt -> a
     where
      foldRecord :: RLProxy ra -> (a -> a -> a) -> Record rt -> Maybe a

instance foldRecordCons
  :: ( IsSymbol s
     , RowToList rt (Cons s t tail)
     , RowCons s t rrt rt
     , FoldRecord ta rrt t
     , RowLacks s rrt
     ) => FoldRecord (Cons k a ta) rt t where
  foldRecord _ f xs = getRes head rest
      where
        name :: SProxy s
        name = SProxy

        head :: t
        head = get name xs

        xs' :: Record rrt
        xs' = delete name xs

        rest = foldRecord (RLProxy :: RLProxy ta) f xs'

        getRes h Nothing = Just h
        getRes h (Just m) = Just (f h m)

instance _foldRecordNil :: FoldRecord Nil a b where
  foldRecord _ _ {} = Nothing

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log <<< unsafeStringify $ fold (<>) { a : "a", b : "b", c : "c" } -- Just "abc"
  log <<< unsafeStringify $ fold (+) { a : 1, b : 4, c : 3 } -- Just 8
  log <<< unsafeStringify $ fold (*) { a : 1, b : 4, c : 3 } -- Just 12
  log <<< unsafeStringify $ fold ((+) :: Int -> Int -> Int) {} -- Nothing
