module MLogo.Interpretation.Command.Commands.Arithmetic.Power
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
  { power: command }

command ∷ Command
command =
  let
    inputParser = ado
      base ← Types.fixedNumberInputParser "base"
      exponent ← Types.fixedNumberInputParser "exponent"
      in { base, exponent }
  in
    Command
      { description: "Rises the base to the given exponent."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "power"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m { base ∷ Number, exponent ∷ Number }
interpret { base, exponent } =
  pure $ Just $ FloatValue $ Number.pow base exponent
