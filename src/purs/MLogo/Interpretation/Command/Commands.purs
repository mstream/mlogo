module MLogo.Interpretation.Command.Commands
  ( commandsByAlias
  , commandsByAliasByCategory
  , parsingContext
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import MLogo.Interpretation.Command.Commands.ControlStructures as ControlStructures
import MLogo.Interpretation.Command.Commands.Graphics as Graphics
import MLogo.Interpretation.Command.Commands.WorkspaceManagement as WorkspaceManagement
import MLogo.Parsing (ParsingContext)

commandsByAliasByCategory ∷ Map String (Map String Command)
commandsByAliasByCategory = Map.fromFoldable
  [ "Arithmetic" /\ Arithmetic.commandsByAlias
  , "Control Structures" /\ ControlStructures.commandsByAlias
  , "Graphics" /\ Graphics.commandsByAlias
  , "Workspace Management" /\ WorkspaceManagement.commandsByAlias
  ]

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  commandsByAliasByCategory

parsingContext ∷ ParsingContext
parsingContext = Command.makeParsingContext commandsByAlias
