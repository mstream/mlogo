module MLogo.Interpretation.Command.Commands.Graphics.SetX
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (modify_)
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
  { setx: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedNumberInputParser "x-coordinate"
  in
    Command
      { description: "Sets cursor's x coordinate."
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "setx"
      , outputValueType: Nothing
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Number
interpret x = pure Nothing <* modify_ \st → st
  { pointer = st.pointer { position = st.pointer.position { x = x } }
  , screen =
      if st.pointer.isDown then
        { color: st.pointer.color
        , p1: st.pointer.position
        , p2: st.pointer.position { x = x }
        } : st.screen
      else st.screen
  }

