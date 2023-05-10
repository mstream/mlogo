module Utils
  ( UriEncodedString
  , decodeFromString
  , decodeFromUriComponent
  , encodeToUriComponent
  , uriEncodedStringToString
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String

foreign import _compressToEncodedURIComponent ∷ String → String
foreign import _decompressFromEncodedURIComponent ∷ String → String

newtype UriEncodedString = UriEncodedString String

uriEncodedStringToString ∷ UriEncodedString → String
uriEncodedStringToString (UriEncodedString s) = s

decodeFromString ∷ String → Maybe String
decodeFromString = Just <<< _decompressFromEncodedURIComponent

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

