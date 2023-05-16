module MLogo.Interpretation.State.Gen (genExecutionState) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenCommon
import Control.Monad.Rec.Class (class MonadRec)
import Data.Map.Gen as MapGen
import Data.String.Gen as StringGen
import MLogo.Interpretation.State (Angle(..), CallStackElement, ExecutionState, PointerState, Position, Procedure, ScreenState, Value(..))
import MLogo.Parsing.Expression (Expression, ParameterName(..))
import MLogo.Parsing.Expression.Gen as ExpressionGen

genExecutionState
  ∷ ∀ m
  . Lazy (m Expression)
  ⇒ MonadGen m
  ⇒ MonadRec m
  ⇒ m ExecutionState
genExecutionState = do
  callStack ← Gen.unfoldable genCallStackElement
  globalVariables ← MapGen.genMap StringGen.genAlphaString genValue
  outputtedValue ← GenCommon.genMaybe genValue
  pointer ← genPointerState
  procedures ← MapGen.genMap StringGen.genAlphaString genProcedure
  repCount ← Gen.chooseInt (-1) 10
  screen ← genScreenState
  pure
    { callStack
    , globalVariables
    , outputtedValue
    , pointer
    , procedures
    , repCount
    , screen
    }

genAngle ∷ ∀ m. MonadGen m ⇒ m Angle
genAngle = Angle <$> Gen.chooseFloat 0.0 359.0

genCallStackElement ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m CallStackElement
genCallStackElement = do
  localVariables ← MapGen.genMap StringGen.genAlphaString genValue
  name ← StringGen.genAlphaString
  pure { localVariables, name }

genProcedure
  ∷ ∀ m. Lazy (m Expression) ⇒ MonadGen m ⇒ MonadRec m ⇒ m Procedure
genProcedure = do
  body ← Gen.unfoldable ExpressionGen.genExpression
  parameterNames ← Gen.unfoldable genParameterName
  pure { body, parameterNames }

genParameterName ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m ParameterName
genParameterName = ParameterName <$> StringGen.genAlphaString

genPointerState ∷ ∀ m. MonadGen m ⇒ m PointerState
genPointerState = do
  angle ← genAngle
  isDown ← Gen.chooseBool
  position ← genPosition
  pure { angle, isDown, position }

genPosition ∷ ∀ m. MonadGen m ⇒ m Position
genPosition = do
  x ← Gen.chooseFloat (-10.0) 10.0
  y ← Gen.chooseFloat (-10.0) 10.0
  pure { x, y }

genScreenState ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m ScreenState
genScreenState = Gen.unfoldable do
  p1 ← genPosition
  p2 ← genPosition
  pure { p1, p2 }

genValue ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Value
genValue = pure $ BooleanValue true

