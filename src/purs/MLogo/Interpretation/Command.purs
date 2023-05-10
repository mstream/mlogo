module MLogo.Interpretation.Command
  ( Command(..)
  , ToMap(..)
  , parseAndInterpretInput
  , makeParsingContext
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Value)
import MLogo.Interpretation.Types (Parameters(..), ValueType)
import MLogo.Parsing (ParsingContext)
import Type.Proxy (Proxy)

newtype Command = Command
  { description ∷ String
  , interpret ∷ ∀ m. Interpret m (List Value)
  , name ∷ String
  , outputValueType ∷ Maybe ValueType
  , parameters ∷ Parameters
  }

parseAndInterpretInput
  ∷ ∀ i m
  . MonadThrow String m
  ⇒ (List Value → String \/ i)
  → (i → m (Maybe Value))
  → List Value
  → m (Maybe Value)
parseAndInterpretInput parseInput interpretInput values =
  case parseInput values of
    Left errorMessage →
      throwError errorMessage
    Right input →
      interpretInput input

makeParsingContext ∷ Map String Command → ParsingContext
makeParsingContext = map commandArity
  where
  commandArity ∷ Command → Maybe Int
  commandArity (Command { parameters }) = case parameters of
    FixedParameters ps →
      Just $ Array.length ps
    VariableParameters _ →
      Nothing

data ToMap = ToMap

instance
  ( IsSymbol sym
  ) ⇒
  FoldingWithIndex
    ToMap
    (Proxy sym)
    (Map String Command)
    Command
    (Map String Command) where
  foldingWithIndex ToMap prop acc val =
    Map.insert (reflectSymbol prop) val acc

