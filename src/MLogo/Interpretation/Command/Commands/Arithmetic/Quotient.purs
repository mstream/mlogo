module MLogo.Interpretation.Command.Commands.Arithmetic.Quotient
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
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
  { equalp: command }

command ∷ Command
command =
  let
    inputParser = ado
      dividend ← Types.fixedNumberInputParser "dividend"
      divisor ← Types.fixedNumberInputParser "divisor"
      in { dividend, divisor }
  in
    Command
      { description: "Divides dividend by divisor."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "quotient"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m { dividend ∷ Number, divisor ∷ Number }
interpret { dividend, divisor } = case divisor of
  0.0 →
    throwError "division by zero"
  x →
    pure $ Just $ FloatValue $ dividend / x
