module Utils
  ( UriEncodedString
  , decodeUriComponentFromString
  , decodeFromUriComponent
  , degreesToRadians
  , encodeToUriComponent
  , genMap
  , uriEncodedStringToString
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String

foreign import _compressToEncodedURIComponent ∷ String → String
foreign import _decompressFromEncodedURIComponent ∷ String → String

newtype UriEncodedString = UriEncodedString String

uriEncodedStringToString ∷ UriEncodedString → String
uriEncodedStringToString (UriEncodedString s) = s

decodeUriComponentFromString ∷ String → Maybe String
decodeUriComponentFromString = Just
  <<< _decompressFromEncodedURIComponent

decodeFromUriComponent ∷ UriEncodedString → String
decodeFromUriComponent (UriEncodedString s) =
  _decompressFromEncodedURIComponent s

encodeToUriComponent ∷ String → Maybe UriEncodedString
encodeToUriComponent s =
  let
    encoded = _compressToEncodedURIComponent s
  in
    if String.length encoded < 1024 then Just $ UriEncodedString encoded
    else Nothing

genMap
  ∷ ∀ k m v. MonadGen m ⇒ MonadRec m ⇒ Ord k ⇒ m k → m v → m (Map k v)
genMap genKey genValue = do
  keys ← Gen.unfoldable genKey
  values ← Gen.unfoldable genValue
  pure $ Map.fromFoldable $ Array.zip keys values

degreesToRadians ∷ Number → Number
degreesToRadians x = x * Number.pi / 180.0
