module MLogo.Interpretation.Command.Commands.Graphics
  ( commandsByAlias
  ) where

import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import MLogo.Interpretation.Command (Command)
import MLogo.Interpretation.Command.Commands.Graphics.Back as Back
import MLogo.Interpretation.Command.Commands.Graphics.Clean as Clean
import MLogo.Interpretation.Command.Commands.Graphics.ClearScreen as ClearScreen
import MLogo.Interpretation.Command.Commands.Graphics.Forward as Forward
import MLogo.Interpretation.Command.Commands.Graphics.Home as Home
import MLogo.Interpretation.Command.Commands.Graphics.Left as Left
import MLogo.Interpretation.Command.Commands.Graphics.PenDown as PenDown
import MLogo.Interpretation.Command.Commands.Graphics.PenUp as PenUp
import MLogo.Interpretation.Command.Commands.Graphics.Right as Right
import MLogo.Interpretation.Command.Commands.Graphics.SetHeading as SetHeading
import MLogo.Interpretation.Command.Commands.Graphics.SetXY as SetXY

commandsByAlias ∷ Map String Command
commandsByAlias = foldl
  Map.union
  Map.empty
  [ Back.commandsByAlias
  , Clean.commandsByAlias
  , ClearScreen.commandsByAlias
  , Forward.commandsByAlias
  , Home.commandsByAlias
  , Left.commandsByAlias
  , PenDown.commandsByAlias
  , PenUp.commandsByAlias
  , Right.commandsByAlias
  , SetHeading.commandsByAlias
  , SetXY.commandsByAlias
  ]

