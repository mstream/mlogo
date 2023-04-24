module MLogo.Interpretation.Command.Commands.Arithmetic.Sin
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

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
  { sin: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "angle"
  in
    Command
      { description:
          "Calculates sinus of a given angle."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "sin"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret = pure
  <<< Just
  <<< FloatValue
  <<< Number.sin
  <<< degreesToRadians

degreesToRadians ∷ Number → Number
degreesToRadians x = x * Number.pi / 180.0
