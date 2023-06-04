module MLogo.WebApp.Utils
  ( UriEncodedString
  , baseUrl
  , classes
  , decodeUriComponentFromString
  , decodeFromUriComponent
  , encodeToUriComponent
  , uriEncodedStringToString
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign, ForeignError)
import Foreign as F
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP

classes
  ∷ ∀ f i r
  . Foldable f
  ⇒ Functor f
  ⇒ f String
  → IProp (class ∷ String | r) i
classes = HP.classes <<< Array.fromFoldable <<< map ClassName

baseUrl ∷ Effect String
baseUrl = do
  v ← _baseUrl
  case stringFromForeign "_baseUrl" v of
    Left errorMessage →
      throw errorMessage
    Right s →
      pure s

newtype UriEncodedString = UriEncodedString String

uriEncodedStringToString ∷ UriEncodedString → String
uriEncodedStringToString (UriEncodedString s) = s

decodeUriComponentFromString ∷ String → Maybe String
decodeUriComponentFromString = hush
  <<< stringFromForeign "_decompressFromEncodedURIComponent"
  <<< _decompressFromEncodedURIComponent

decodeFromUriComponent ∷ UriEncodedString → Maybe String
decodeFromUriComponent (UriEncodedString s) = hush
  $ stringFromForeign "_decompressFromEncodedURIComponent"
  $ _decompressFromEncodedURIComponent s

encodeToUriComponent ∷ String → Maybe UriEncodedString
encodeToUriComponent s = do
  encoded ← hush
    $ stringFromForeign "_compressToEncodedURIComponent"
    $ _compressToEncodedURIComponent s

  if String.length encoded < 1024 then Just $ UriEncodedString encoded
  else Nothing

stringFromForeign ∷ String → Foreign → String \/ String
stringFromForeign name v = case result of
  Left foreignErrors →
    Left $ errorMessage foreignErrors
  Right s →
    Right s
  where
  result ∷ NonEmptyList ForeignError \/ String
  result = runExcept $ F.readString v

  errorMessage ∷ NonEmptyList ForeignError → String
  errorMessage errors = "Unexpected foreign value \""
    <> name
    <> "\": "
    <> String.joinWith ","
      (Array.fromFoldable $ F.renderForeignError <$> errors)

foreign import _baseUrl ∷ Effect Foreign
foreign import _compressToEncodedURIComponent ∷ String → Foreign
foreign import _decompressFromEncodedURIComponent ∷ String → Foreign
