module Bundaegi.Internal where

import Data.Generic.Rep (Constructor, Field, Product, Sum)
import Type.Prelude (class TypeEquals, RLProxy)
import Type.Row (Cons, Nil, kind RowList)

class FieldsToRow fields (row :: # Type)

instance fieldFieldsToRow ::
  ( RowCons name ty () row
  ) => FieldsToRow (Field name ty) row

instance productFieldsToRow ::
  ( FieldsToRow a l
  , FieldsToRow b r
  , Union l r row
  ) => FieldsToRow (Product a b) row

class GenericSumToRowList a (rl :: RowList)

instance sumGenericSumToRowList ::
  ( GenericSumToRowList a l
  , GenericSumToRowList b r
  , RowListAppend l r rl
  ) => GenericSumToRowList (Sum a b) rl

instance constructorGenericSumToRowList ::
  ( RowListAppend Nil (Cons name ty Nil) rl
  ) => GenericSumToRowList (Constructor name ty) rl

-- https://github.com/LiamGoodacre/purescript-typelevel-prelude/blob/7fba5aab064f4f3b7fb05a183404abcca4d7d84d/src/Type/Row.purs
-- Append two row lists together
class RowListAppend (lhs :: RowList)
                    (rhs :: RowList)
                    (out :: RowList)
                    | lhs rhs -> out

instance rowListAppendNil
  :: TypeEquals (RLProxy rhs) (RLProxy out)
  => RowListAppend Nil rhs out

instance rowListAppendCons
  :: ( RowListAppend tail rhs out'
     , TypeEquals (RLProxy (Cons label head out')) (RLProxy out) )
  => RowListAppend (Cons label head tail) rhs out
