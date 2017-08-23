module Bundaegi where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (Cons, Nil, kind RowList)

type Indents = Int

getIndents :: Int -> String
getIndents 0 = ""
getIndents n = "  " <> getIndents (n - 1)

getTSRep :: forall a
   . HasTSRep a
  => String -> Proxy a -> String
getTSRep name p = "type " <> name <> " = " <> toTSRep 0 p

class HasTSRep a where
  toTSRep :: Indents -> Proxy a -> String

instance numberHasTSRep :: HasTSRep Number where
  toTSRep _ _ = "number"
instance stringHasTSRep :: HasTSRep String where
  toTSRep _ _ = "string"
instance booleanHasTSRep :: HasTSRep Boolean where
  toTSRep _ _ = "boolean"
instance arrayHasTSRep :: HasTSRep a => HasTSRep (Array a) where
  toTSRep i _ = toTSRep i p <> "[]"
    where
      p = Proxy :: Proxy a
instance recordHasTSRep ::
  ( RowToList row rl
  , HasTSRepFields rl
  ) => HasTSRep (Record row) where
  toTSRep i _ = "{\n" <> indents <> "  " <> fields <> "\n" <> indents <> "}"
    where
      indents = getIndents i
      rlp = RLProxy :: RLProxy rl
      fields = intercalate ("\n" <> indents <> "  ") $ toTSRepFields (i + 2) rlp

class HasTSRepFields (rl :: RowList) where
  toTSRepFields :: Indents -> RLProxy rl -> List String

instance consHasTSRepFields ::
  ( IsSymbol name
  , HasTSRep ty
  , HasTSRepFields tail
  ) => HasTSRepFields (Cons name ty tail) where
  toTSRepFields i _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      value = toTSRep (i + 1) typ
      head = key <> " : " <> value
      tailp = RLProxy :: RLProxy tail
      tail = toTSRepFields i tailp

instance nilHasTSRepFields :: HasTSRepFields Nil where
  toTSRepFields _ _ = mempty

genericToTSRep :: forall a rep
   . Generic a rep
  => GenericHasTSRep rep
  => Indents -> Proxy a -> String
genericToTSRep i _ = genericToTSRepImpl i (Proxy :: Proxy rep)

class GenericHasTSRep a where
  genericToTSRepImpl :: Indents -> Proxy a -> String

instance noArgumentsHasTSRep :: HasTSRep NoArguments where
  toTSRep _ _ = "never"

instance argumentsHasTSRep ::
  ( HasTSRep a
  ) => HasTSRep (Argument a) where
  toTSRep i _ = toTSRep i p
    where
      p = Proxy :: Proxy a

instance constructorGenericHasTSRep ::
  ( IsSymbol name
  , HasTSRep ty
  ) => GenericHasTSRep (Constructor name ty) where
  genericToTSRepImpl i _ =
    "{\n" <> indents <> "  tag : \"" <> tag <> "\""
      <> "\n" <> indents <> "  content : " <> content
      <> "\n" <> indents <> "}"
    where
      indents = getIndents (i + 1)
      namep = SProxy :: SProxy name
      tag = reflectSymbol namep
      typ = Proxy :: Proxy ty
      content = toTSRep (i + 1) typ

instance sumGenericHasTSRep ::
  ( GenericHasTSRep a
  , GenericHasTSRep b
  ) => GenericHasTSRep (Sum a b) where
  genericToTSRepImpl i _ =
    "\n" <> indent <> "| " <> a
      <> "\n" <> indent <> "| " <> b
    where
      indent = getIndents (i + 1)
      a = genericToTSRepImpl (i + 1) (Proxy :: Proxy a)
      b = genericToTSRepImpl (i + 1) (Proxy :: Proxy b)

instance productGenericHasTSRep ::
  ( Fail "I'm not going to deal with encoding product types in TS, use a record"
  ) => GenericHasTSRep (Product a b) where
  genericToTSRepImpl _ _ = unsafeCrashWith "unreachable product impl"
