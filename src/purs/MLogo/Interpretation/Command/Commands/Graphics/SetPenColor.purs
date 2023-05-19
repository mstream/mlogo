module MLogo.Interpretation.Command.Commands.Graphics.SetPenColor
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get, modify_)
import Data.Either (Either(..))
import Data.List ((:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Heterogeneous.Folding as Heterogeneous
import MLogo.Interpretation.Command (Command(..), ToMap(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Interpret (Interpret)
import MLogo.Interpretation.Types as Types

commandsByAlias ∷ Map String Command
commandsByAlias = Heterogeneous.hfoldlWithIndex
  ToMap
  (Map.empty ∷ Map String Command)
  { setpc: command
  , setpencolor: command
  }

command ∷ Command
command =
  let
    inputParser = Types.fixedIntInputParser "color palette index"
  in
    Command
      { description:
          "Sets pen's color to a palette color under a given index ."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "setpencolor"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Int
interpret n = do
  { colorPalette } ← get
  pure Nothing <* case Map.lookup n colorPalette of
    Just color →
      modify_ \st → st { pointer = st.pointer { color = color } }
    Nothing →
      throwError "selected color does not exist in the color palette"

