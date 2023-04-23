module MLogo.Interpretation.Command.Commands.Arithmetic
  ( commandsByAlias
  , isEqual
  , sinus
  , sum
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.State (Value(..))
import MLogo.Interpretation.Types (ValueType(..))
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { equalp: isEqual
  , sin: sinus
  , sum
  }

sinus ∷ Command
sinus =
  let
    inputParser = Types.fixedNumberInputParser "angle"
  in
    Command
      { description:
          "Calculates sinus of a given angle."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          ( pure
              <<< Just
              <<< FloatValue
              <<< Number.sin
              <<< degreesToRadians
          )
      , name: "sin"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

sum ∷ Command
sum =
  let
    inputParser = Types.variableNumberInputParser "addend"
  in
    Command
      { description: "Sums up given numbers."
      , interpret: Command.parseAndInterpretInput
          (Types.runVariableInputParser inputParser)
          interpretSum
      , name: "sum"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromVariableInputParser inputParser
      }

isEqual ∷ Command
isEqual =
  let
    inputParser = Types.variableAnyInputParser "value"
  in
    Command
      { description: "Tells if values are equal."
      , interpret: \values →
          case Types.runVariableInputParser inputParser values of
            Left errorMessage →
              throwError errorMessage
            Right input →
              interpretIsEqual input
      , name: "equalp"
      , outputValueType: Just BooleanType
      , parameters: Types.parametersFromVariableInputParser inputParser
      }

interpretSum ∷ ∀ m. Interpret m (List Number)
interpretSum = pure <<< Just <<< FloatValue <<< foldl (+) zero

interpretIsEqual ∷ ∀ m. Interpret m (List Value)
interpretIsEqual = pure <<< Just <<< BooleanValue <<< go true Nothing
  where
  go ∷ Boolean → (Maybe Value) → (List Value) → Boolean
  go acc mbLast = case _ of
    Nil →
      acc
    v : vs →
      let
        continue = go acc (Just v) vs
      in
        case mbLast of
          Nothing →
            continue
          Just last →
            if v == last then
              continue
            else
              false

degreesToRadians ∷ Number → Number
degreesToRadians x = x * Number.pi / 180.0

