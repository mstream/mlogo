module MLogo.Interpretation.Command.Commands.WorkspaceManagement
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.WorkspaceManagement.Make as Make
import MLogo.Interpretation.Command.Commands.WorkspaceManagement.RepCount as RepCount

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Make.commandsByAlias
  , RepCount.commandsByAlias
  ]
