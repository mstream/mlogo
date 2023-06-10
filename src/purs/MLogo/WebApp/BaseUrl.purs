module MLogo.WebApp.BaseUrl
  ( BaseUrl
  , fromSegments
  , segments
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNonEmpty
import Data.Unfoldable (class Unfoldable)

newtype BaseUrl = BaseUrl (Array NonEmptyString)

fromSegments ∷ ∀ f. Foldable f ⇒ f NonEmptyString → BaseUrl
fromSegments = BaseUrl <<< Array.fromFoldable

segments ∷ ∀ f. Unfoldable f ⇒ BaseUrl → f String
segments (BaseUrl ss) = Array.toUnfoldable
  $ StringNonEmpty.toString <$> ss

