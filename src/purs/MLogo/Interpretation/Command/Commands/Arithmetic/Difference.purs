module MLogo.Interpretation.Command.Commands.Arithmetic.Difference
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
  { difference: command }

command ∷ Command
command =
  let
    inputParser = ado
      minuend ← Types.fixedNumberInputParser "minuend"
      subtrahend ← Types.fixedNumberInputParser "subtrahend"
      in { minuend, subtrahend }
  in
    Command
      { description: "Subtracts subtrahend from minuend."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "difference"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m { minuend ∷ Number, subtrahend ∷ Number }
interpret { minuend, subtrahend } =
  pure $ Just $ FloatValue $ minuend - subtrahend
