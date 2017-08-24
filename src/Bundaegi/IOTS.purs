module Bundaegi.IOTS where

import Prelude

import Bundaegi.Internal (class FieldsToRow, class GenericSumToRowList, class RowListAppend)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Rec, Sum)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.String (null)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (Cons, Nil, kind RowList)

getIOTSRep :: forall a
   . HasIOTSRep a
  => String -> Proxy a -> String
getIOTSRep name p = "export const " <> name <> "=" <> toIOTSRep p

class HasIOTSRep a where
  toIOTSRep :: Proxy a -> String

instance numberHasIOTSRep :: HasIOTSRep Number where
  toIOTSRep _ = "t.number"
instance stringHasIOTSRep :: HasIOTSRep String where
  toIOTSRep _ = "t.string"
instance booleanHasIOTSRep :: HasIOTSRep Boolean where
  toIOTSRep _ = "t.boolean"
instance arrayHasIOTSRep :: HasIOTSRep a => HasIOTSRep (Array a) where
  toIOTSRep _ = "t.array(" <> toIOTSRep p <> ")"
    where
      p = Proxy :: Proxy a
instance recordHasIOTSRep ::
  ( RowToList row rl
  , HasIOTSRepFields rl
  ) => HasIOTSRep (Record row) where
  toIOTSRep _ = "t[\"interface\"]({" <> fields <> "})"
    where
      rlp = RLProxy :: RLProxy rl
      fields = intercalate "," $ toIOTSRepFields rlp

class HasIOTSRepFields (rl :: RowList) where
  toIOTSRepFields :: RLProxy rl -> List String

instance consHasIOTSRepFields ::
  ( IsSymbol name
  , HasIOTSRep ty
  , HasIOTSRepFields tail
  ) => HasIOTSRepFields (Cons name ty tail) where
  toIOTSRepFields _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      value = toIOTSRep typ
      head = key <> ":" <> value
      tailp = RLProxy :: RLProxy tail
      tail = toIOTSRepFields tailp

instance nilHasIOTSRepFields :: HasIOTSRepFields Nil where
  toIOTSRepFields _ = mempty

genericToIOTSRep :: forall a rep
   . Generic a rep
  => GenericHasIOTSRep rep
  => Proxy a -> String
genericToIOTSRep _ = genericToIOTSRepImpl (Proxy :: Proxy rep)

class GenericHasIOTSRep a where
  genericToIOTSRepImpl :: Proxy a -> String

instance noArgumentsHasIOTSRep :: HasIOTSRep NoArguments where
  toIOTSRep _ = "" -- rotten value to indicate we have nothing to write

instance argumentsHasIOTSRep ::
  ( HasIOTSRep a
  ) => HasIOTSRep (Argument a) where
  toIOTSRep _ = toIOTSRep p
    where
      p = Proxy :: Proxy a

instance constructorGenericHasIOTSRep ::
  ( IsSymbol name
  , HasIOTSRep ty
  ) => GenericHasIOTSRep (Constructor name ty) where
  genericToIOTSRepImpl _ =
    "t[\"interface\"]({" <> "tag: t.literal(\"" <> tag <> "\")"
      <> if null content -- if nothing is given back, we won't print it
            then "})"
            else ",content:" <> content <> "})"
    where
      namep = SProxy :: SProxy name
      tag = reflectSymbol namep
      typ = Proxy :: Proxy ty
      content = toIOTSRep typ

instance sumGenericHasIOTSRep ::
  ( GenericSumToRowList a l
  , GenericSumToRowList b r
  , RowListAppend l r rl
  , HasIOTSRepUnionList rl
  ) => GenericHasIOTSRep (Sum a b) where
  genericToIOTSRepImpl _ =
    "t.union([" <> fields <> "])"
    where
      rlp = RLProxy :: RLProxy rl
      xs = unionListToIOTSRep rlp
      fields = intercalate "," $ xs

instance productGenericHasIOTSRep ::
  ( Fail "I'm not going to deal with encoding product types in IOTS, use a record"
  ) => GenericHasIOTSRep (Product a b) where
  genericToIOTSRepImpl _ = unsafeCrashWith "unreachable product impl"

instance recHasIOTSRep ::
  ( FieldsToRow fields row
  , RowToList row ls
  , HasIOTSRep (Record row)
  ) => HasIOTSRep (Rec fields) where
  toIOTSRep _ = toIOTSRep p
    where
      p = Proxy :: Proxy (Record row)

class HasIOTSRepUnionList (rl :: RowList) where
  unionListToIOTSRep :: RLProxy rl -> List String

instance nilHasIOTSRepUnionList :: HasIOTSRepUnionList Nil where
  unionListToIOTSRep _ = mempty

instance consHasIOTSRepUnionList ::
  ( HasIOTSRepUnionList tail
  , GenericHasIOTSRep (Constructor name ty)
  ) => HasIOTSRepUnionList (Cons name ty tail) where
  unionListToIOTSRep _ = head : tail
    where
      p = Proxy :: Proxy (Constructor name ty)
      head = genericToIOTSRepImpl p
      tailp = RLProxy :: RLProxy tail
      tail = unionListToIOTSRep tailp
