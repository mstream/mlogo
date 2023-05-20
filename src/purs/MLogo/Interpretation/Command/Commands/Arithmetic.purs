module MLogo.Interpretation.Command.Commands.Arithmetic
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.Arithmetic.Cos as Cos
import MLogo.Interpretation.Command.Commands.Arithmetic.Difference as Difference
import MLogo.Interpretation.Command.Commands.Arithmetic.EqualP as EqualP
import MLogo.Interpretation.Command.Commands.Arithmetic.Minus as Minus
import MLogo.Interpretation.Command.Commands.Arithmetic.Power as Power
import MLogo.Interpretation.Command.Commands.Arithmetic.Product as Product
import MLogo.Interpretation.Command.Commands.Arithmetic.Quotient as Quotient
import MLogo.Interpretation.Command.Commands.Arithmetic.Random as Random
import MLogo.Interpretation.Command.Commands.Arithmetic.ReRandom as ReRandom
import MLogo.Interpretation.Command.Commands.Arithmetic.Sin as Sin
import MLogo.Interpretation.Command.Commands.Arithmetic.Sum as Sum

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Cos.commandsByAlias
  , Difference.commandsByAlias
  , EqualP.commandsByAlias
  , Minus.commandsByAlias
  , Power.commandsByAlias
  , Product.commandsByAlias
  , Random.commandsByAlias
  , ReRandom.commandsByAlias
  , Quotient.commandsByAlias
  , Sin.commandsByAlias
  , Sum.commandsByAlias
  ]
