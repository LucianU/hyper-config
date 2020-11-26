module HyperWidgetConfig.Reify
  ( RuntimeType(..)
  , class Reify
  , unreify
  , reify
  , reifyRecord'
  , reifySubRecord'
  , class ReifyRowList
  , reifyRowList
  ) where

import Prelude

import Data.Eq (class EqRecord)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.SubRecord (class EqSubRecord, SubRecord)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (class VariantEqs, Variant)
import Data.Variant.Internal (class VariantTags)
import Foreign.Object (Object)
import Foreign.Object as Object
import HyperWidgetConfig.Reference (Reference)
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

-- | Poor-man's type reflection. Provides runtime information
-- | about the types to help with the dynamic Record traversal.
data RuntimeType
  = RuntimeInt
  | RuntimeNumber
  | RuntimeString
  | RuntimeUnit
  | RuntimeReference RuntimeType
  | RuntimeVariant (Object RuntimeType)
  | RuntimeRecord (Object RuntimeType)

derive instance eqRuntimeType :: Eq RuntimeType

-- can't do it with Generic because of recursive type
instance showRuntimeType :: Show RuntimeType where
  show = case _ of
    RuntimeInt -> "RuntimeInt"
    RuntimeNumber -> "RuntimeNumber"
    RuntimeString -> "RuntimeString"
    RuntimeUnit -> "RuntimeUnit"
    RuntimeReference runtimeType -> "(RuntimeReference " <> show runtimeType <> ")"
    RuntimeVariant obj -> "(RuntimeVariant " <> show obj <> ")"
    RuntimeRecord obj -> "(RuntimeRecord " <> show obj <> ")"

unreify :: forall a. Reify a => RuntimeType -> Maybe (Proxy a)
unreify r
  | reify (Proxy :: Proxy a) == r = Just (Proxy :: Proxy a)
  | otherwise = Nothing

-- | WARN: Must be a 1-to-1 correspondance. It is prohibited to reify two different
-- | purescript types as the same runtime type.
class
  Eq a <= Reify a where
  reify :: Proxy a -> RuntimeType

instance reifyInt :: Reify Int where
  reify _ = RuntimeInt

instance reifyNumber :: Reify Number where
  reify _ = RuntimeNumber

instance reifyString :: Reify String where
  reify _ = RuntimeString

instance reifyUnit :: Reify Unit where
  reify _ = RuntimeUnit

instance reifyReference :: Reify a => Reify (Reference a) where
  reify _ = RuntimeReference $ reify (Proxy :: Proxy a)

instance reifyVariant :: (RL.RowToList row list, VariantTags list, VariantEqs list, ReifyRowList list) => Reify (Variant row) where
  reify = RuntimeVariant <<< reifyVariant'

instance reifyRecord :: (EqRecord list row, RL.RowToList row list, ReifyRowList list) => Reify (Record row) where
  reify = RuntimeRecord <<< reifyRecord'

instance reifySubRecord :: (EqSubRecord list row, RL.RowToList row list, ReifyRowList list) => Reify (SubRecord row) where
  reify = RuntimeRecord <<< reifySubRecord'

reifyRecord' :: forall row list. RL.RowToList row list => ReifyRowList list => Proxy (Record row) -> Object RuntimeType
reifyRecord' _ = Object.fromFoldable (reifyRowList (RLProxy :: RLProxy list))

reifySubRecord' :: forall row list. RL.RowToList row list => ReifyRowList list => Proxy (SubRecord row) -> Object RuntimeType
reifySubRecord' _ = reifyRecord' (Proxy :: Proxy (Record row))

reifyVariant' :: forall row list. RL.RowToList row list => ReifyRowList list => Proxy (Variant row) -> Object RuntimeType
reifyVariant' _ = reifyRecord' (Proxy :: Proxy (Record row))

class ReifyRowList rowlist where
  reifyRowList :: RLProxy rowlist -> List (Tuple String RuntimeType)

instance reifyRowListNil :: ReifyRowList RL.Nil where
  reifyRowList _ = Nil

instance reifyRowListCons ::
  ( IsSymbol key
  , ReifyRowList rowlistTail
  , Reify focus
  ) =>
  ReifyRowList (RL.Cons key focus rowlistTail) where
  reifyRowList _ = (Tuple key focus) : tail
    where
    key = reflectSymbol (SProxy :: SProxy key)

    focus = reify (Proxy :: Proxy focus)

    tail = reifyRowList (RLProxy :: RLProxy rowlistTail)
