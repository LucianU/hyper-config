module HyperWidgetConfig.Resolve.ConfigTree.Value
  ( Value
  , mkValue
  , getType
  , fromValue
  , Any
  , mkAny
  , runAny
  , unsafeFromValue
  ) where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Global.Unsafe (unsafeStringify)
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Reify (class Reify, RuntimeType(..), reify, unreify)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Any
  = Exists Identity

mkAny :: forall a. a -> Any
mkAny = mkExists <<< Identity

runAny :: forall r. (forall a. a -> r) -> Any -> r
runAny f = runExists (f <<< unwrap)

data Value
  = Value RuntimeType Any

instance eqValue :: Eq Value where
  eq (Value t a) (Value t' b)
    | t == t' = case t of
      RuntimeInt -> runAny unsafeCoerce a == (runAny unsafeCoerce b :: Int)
      RuntimeNumber -> runAny unsafeCoerce a == (runAny unsafeCoerce b :: Number)
      RuntimeVariant _ -> unsafeStringify a == unsafeStringify b
      RuntimeRecord _ -> unsafeStringify a == unsafeStringify b -- WARN: not ideal, but how do we check for it anyway
      RuntimeString -> runAny unsafeCoerce a == (runAny unsafeCoerce b :: String)
      RuntimeUnit -> runAny unsafeCoerce a == (runAny unsafeCoerce b :: Unit)
      RuntimeReference r ->
        let
          ra = unsafeCoerce a :: Reference Any

          rb = unsafeCoerce b :: Reference Any
        in
          case ra, rb of
            Reference stra, Reference strb -> stra == strb
            Literal la, Literal lb -> eq (Value r la) (Value r lb)
            _, _ -> false
    | otherwise = false

instance showValue :: Show Value where
  show a'@(Value t a) = "(Value " <> show t <> " " <> showTypedAny t a <> ")"

showTypedAny :: RuntimeType -> Any -> String
showTypedAny t a = case t of
  RuntimeInt -> show $ (runAny unsafeCoerce a :: Int)
  RuntimeNumber -> show $ (runAny unsafeCoerce a :: Number)
  RuntimeString -> runAny unsafeCoerce a :: String
  RuntimeUnit -> show $ (runAny unsafeCoerce a :: Unit)
  RuntimeReference r -> case unsafeCoerce a :: Reference Any of
    Reference str -> "(Reference " <> str <> ")"
    Literal la -> showTypedAny r la
  RuntimeVariant _ -> unsafeStringify a
  RuntimeRecord _ -> unsafeStringify a -- WARN: not ideal, but how do we do that anyway

mkValue :: forall a. Reify a => a -> Value
mkValue a = Value (reify (Proxy :: Proxy a)) (mkAny a)

getType :: Value -> RuntimeType
getType (Value t _) = t

fromValue :: forall a. Reify a => Value -> Maybe a
fromValue (Value t a) = runAny f a
  where
  f :: forall x. x -> Maybe a
  f x = unsafeCoerce x <$ (unreify t :: Maybe (Proxy a))

unsafeFromValue :: Value -> Any
unsafeFromValue (Value t a) = a
