module MLogo.Interpretation.Command.Commands.Arithmetic
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import MLogo.Interpretation.Command.Commands.Arithmetic.Product as Product
import MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Difference.commandsByAlias
  , EqualP.commandsByAlias
  , Power.commandsByAlias
  , Product.commandsByAlias
  , Quotient.commandsByAlias
  , Sin.commandsByAlias
  , Sum.commandsByAlias
  ]
