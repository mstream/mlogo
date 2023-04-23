module MLogo.Interpretation.Command.Commands
  ( commandsByAlias
  , parsingContext
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command(..))
import MLogo.Interpretation.Command as Command
import MLogo.Interpretation.Command.Commands.Arithmetic as Arithmetic
import MLogo.Interpretation.Command.Commands.Graphics as Graphics
import MLogo.Interpretation.Command.Commands.WorkspaceManagement as WorkspaceManagement
import MLogo.Parsing (ParsingContext)

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Arithmetic.commandsByAlias
  , Graphics.commandsByAlias
  , WorkspaceManagement.commandsByAlias
  ]

parsingContext ∷ ParsingContext
parsingContext = Command.makeParsingContext commandsByAlias
