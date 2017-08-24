module Bundaegi where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic, Argument, Constructor, Field, NoArguments, Product, Rec, Sum)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (Cons, Nil, kind RowList)

getTSRep :: forall a
   . HasTSRep a
  => String -> Proxy a -> String
getTSRep name p = "type " <> name <> "=" <> toTSRep p

class HasTSRep a where
  toTSRep :: Proxy a -> String

instance numberHasTSRep :: HasTSRep Number where
  toTSRep _ = "number"
instance stringHasTSRep :: HasTSRep String where
  toTSRep _ = "string"
instance booleanHasTSRep :: HasTSRep Boolean where
  toTSRep _ = "boolean"
instance arrayHasTSRep :: HasTSRep a => HasTSRep (Array a) where
  toTSRep _ = toTSRep p <> "[]"
    where
      p = Proxy :: Proxy a
instance recordHasTSRep ::
  ( RowToList row rl
  , HasTSRepFields rl
  ) => HasTSRep (Record row) where
  toTSRep _ = "{" <> fields <> "}"
    where
      rlp = RLProxy :: RLProxy rl
      fields = intercalate "," $ toTSRepFields rlp

class HasTSRepFields (rl :: RowList) where
  toTSRepFields :: RLProxy rl -> List String

instance consHasTSRepFields ::
  ( IsSymbol name
  , HasTSRep ty
  , HasTSRepFields tail
  ) => HasTSRepFields (Cons name ty tail) where
  toTSRepFields _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      value = toTSRep typ
      head = key <> ":" <> value
      tailp = RLProxy :: RLProxy tail
      tail = toTSRepFields tailp

instance nilHasTSRepFields :: HasTSRepFields Nil where
  toTSRepFields _ = mempty

genericToTSRep :: forall a rep
   . Generic a rep
  => GenericHasTSRep rep
  => Proxy a -> String
genericToTSRep _ = genericToTSRepImpl (Proxy :: Proxy rep)

class GenericHasTSRep a where
  genericToTSRepImpl :: Proxy a -> String

instance noArgumentsHasTSRep :: HasTSRep NoArguments where
  toTSRep _ = "any" -- how the fuck do you make this nothing that works??

instance argumentsHasTSRep ::
  ( HasTSRep a
  ) => HasTSRep (Argument a) where
  toTSRep _ = toTSRep p
    where
      p = Proxy :: Proxy a

instance constructorGenericHasTSRep ::
  ( IsSymbol name
  , HasTSRep ty
  ) => GenericHasTSRep (Constructor name ty) where
  genericToTSRepImpl _ =
    "{" <> "tag:\"" <> tag <> "\""
      <> ",content:" <> content <> "}"
    where
      namep = SProxy :: SProxy name
      tag = reflectSymbol namep
      typ = Proxy :: Proxy ty
      content = toTSRep typ

instance sumGenericHasTSRep ::
  ( GenericHasTSRep a
  , GenericHasTSRep b
  ) => GenericHasTSRep (Sum a b) where
  genericToTSRepImpl _ =
    a <> "|" <> b
    where
      a = genericToTSRepImpl (Proxy :: Proxy a)
      b = genericToTSRepImpl (Proxy :: Proxy b)

instance productGenericHasTSRep ::
  ( Fail "I'm not going to deal with encoding product types in TS, use a record"
  ) => GenericHasTSRep (Product a b) where
  genericToTSRepImpl _ = unsafeCrashWith "unreachable product impl"

instance recHasTSRep ::
  ( FieldsToRow fields row
  , RowToList row ls
  , HasTSRep (Record row)
  ) => HasTSRep (Rec fields) where
  toTSRep _ = toTSRep p
    where
      p = Proxy :: Proxy (Record row)

class FieldsToRow fields (row :: # Type)

instance fieldFieldsToRow ::
  ( RowCons name ty () row
  ) => FieldsToRow (Field name ty) row

instance productFieldsToRow ::
  ( FieldsToRow a l
  , FieldsToRow b r
  , Union l r row
  ) => FieldsToRow (Product a b) row