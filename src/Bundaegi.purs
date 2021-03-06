module Bundaegi where

import Prelude

import Bundaegi.Internal (class FieldsToRow, class GenericSumToRowList)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Rec, Sum)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.String (null)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (Cons, Nil, kind RowList)

getTSRep :: forall a
   . HasTSRep a
  => String -> Proxy a -> String
getTSRep name p = "export type " <> name <> "=" <> toTSRep p

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
  toTSRep _ = "" -- rotten value to indicate we have nothing to write

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
      <> if null content -- if nothing is given back, we won't print it
            then "}"
            else ",content:" <> content <> "}"
    where
      namep = SProxy :: SProxy name
      tag = reflectSymbol namep
      typ = Proxy :: Proxy ty
      content = toTSRep typ

instance sumGenericHasTSRep ::
  ( GenericSumToRowList (Sum a b) rl
  , HasTSRepUnionList rl
  ) => GenericHasTSRep (Sum a b) where
  genericToTSRepImpl _ =
    intercalate "|" members
    where
      rlp = RLProxy :: RLProxy rl
      members = unionListToTSRep rlp

instance productGenericHasTSRep ::
  ( Fail "I'm not going to deal with encoding product types in TS, use a record"
  ) => GenericHasTSRep (Product a b) where
  genericToTSRepImpl _ = unsafeCrashWith "unreachable product impl"

instance recHasTSRep ::
  ( FieldsToRow fields row
  , HasTSRep (Record row)
  ) => HasTSRep (Rec fields) where
  toTSRep _ = toTSRep p
    where
      p = Proxy :: Proxy (Record row)

class HasTSRepUnionList (rl :: RowList) where
  unionListToTSRep :: RLProxy rl -> List String

instance nilHasTSRepUnionList :: HasTSRepUnionList Nil where
  unionListToTSRep _ = mempty

instance consHasTSRepUnionList ::
  ( HasTSRepUnionList tail
  , GenericHasTSRep (Constructor name ty)
  ) => HasTSRepUnionList (Cons name ty tail) where
  unionListToTSRep _ = head : tail
    where
      p = Proxy :: Proxy (Constructor name ty)
      head = genericToTSRepImpl p
      tailp = RLProxy :: RLProxy tail
      tail = unionListToTSRep tailp
