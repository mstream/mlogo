module Utils (degreesToRadians, genMap) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Number as Number

genMap
  ∷ ∀ k m v. MonadGen m ⇒ MonadRec m ⇒ Ord k ⇒ m k → m v → m (Map k v)
genMap genKey genValue = do
  keys ← Gen.unfoldable genKey
  values ← Gen.unfoldable genValue
  pure $ Map.fromFoldable $ Array.zip keys values

degreesToRadians ∷ Number → Number
degreesToRadians x = x * Number.pi / 180.0
