module MLogo.Interpretation.Command.Commands.Arithmetic
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ EqualP.commandsByAlias
  , Sin.commandsByAlias
  , Sum.commandsByAlias
  ]
