module MLogo.Interpretation.Command.Commands.ControlStructures
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.ControlStructures.Output as Output

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Output.commandsByAlias
  ]
