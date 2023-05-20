module MLogo.Interpretation.Command.Commands.Arithmetic.Random
  ( command
  , commandsByAlias
  , interpret
  ) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Int as Int
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
  { random: command }

command ∷ Command
command =
  let
    inputParser = Types.fixedIntInputParser "limit"
  in
    Command
      { description:
          "outputs a random between 0 (inclusively) and a given limit (exclusively)"
      , interpret: Command.parseAndInterpretInput
          (Types.runFixedInputParser inputParser)
          interpret
      , name: "random"
      , outputValueType: Just NumberType
      , parameters: Types.parametersFromFixedInputParser inputParser
      }

interpret ∷ ∀ m. Interpret m Int
interpret upperLimit = do
  { randomNumberSeed } ← get

  let
    newSeed = (multiplier * randomNumberSeed + increment) `mod` modulus

  modify_ _ { randomNumberSeed = newSeed }
  pure $ Just $ IntegerValue $ newSeed `mod` upperLimit

increment ∷ Int
increment = 12345

modulus ∷ Int
modulus = Int.trunc 2e31

multiplier ∷ Int
multiplier = 1103515245

