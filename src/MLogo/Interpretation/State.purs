module MLogo.Interpretation.State
  ( Angle(..)
  , ExecutionState
  , Line
  , PointerState
  , Position(..)
  , ScreenState
  , Value(..)
  , extractBoolean
  , extractNumber
  , extractString
  , initialExecutionState
  , toRadians
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Number as Number
import Data.Show.Generic (genericShow)
import MLogo.Parsing (Parameter, Statement)

data Value
  = BooleanValue Boolean
  | NumberValue Number
  | WordValue String

derive instance Generic Value _

derive instance Eq Value

instance Show Value where
  show = genericShow

newtype Angle = Angle Number

derive newtype instance Eq Angle
derive newtype instance Show Angle
derive newtype instance Semiring Angle
derive newtype instance Ring Angle
derive newtype instance EncodeJson Angle

extractBoolean ∷ Value → String \/ Boolean
extractBoolean = case _ of
  BooleanValue b →
    Right b
  otherValue →
    Left $ "\"" <> show otherValue <> "\" is not a boolean value"

extractNumber ∷ Value → String \/ Number
extractNumber = case _ of
  NumberValue x →
    Right x
  otherValue →
    Left $ "\"" <> show otherValue <> "\" is not a number value"

extractString ∷ Value → String \/ String
extractString = case _ of
  WordValue s →
    Right s
  otherValue →
    Left $ "\"" <> show otherValue <> "\" is not a word value"

toRadians ∷ Angle → Number
toRadians (Angle x) = x * Number.pi / 180.0

newtype Steps = Steps Int

derive newtype instance Eq Steps
derive newtype instance Show Steps

derive newtype instance Semiring Steps
derive newtype instance Ring Steps

type PointerState =
  { angle ∷ Angle
  , isDown ∷ Boolean
  , position ∷ Position
  }

initialPointerState ∷ PointerState
initialPointerState =
  { angle: zero
  , isDown: true
  , position: zero
  }

type ScreenState = List Line

type ExecutionState =
  { callStack ∷
      List
        { name ∷ String
        , boundArguments ∷ Map Parameter Value
        }
  , pointer ∷ PointerState
  , procedures ∷
      Map String
        { body ∷ List Statement
        , parameters ∷ List Parameter
        }
  , screen ∷ ScreenState
  , variables ∷ Map String Value
  }

initialExecutionState ∷ ExecutionState
initialExecutionState =
  { callStack: Nil
  , pointer: initialPointerState
  , procedures: Map.empty
  , screen: Nil
  , variables: Map.empty
  }

newtype Position = Position { x ∷ Number, y ∷ Number }

derive instance Generic Position _
derive newtype instance Eq Position
derive newtype instance Show Position
derive newtype instance EncodeJson Position

instance Semiring Position where
  add (Position p1) (Position p2) = Position
    { x: p1.x + p2.x, y: p1.y + p2.y }
  mul (Position p1) (Position p2) = Position
    { x: p1.x * p2.x, y: p1.y * p2.y }
  one = Position one
  zero = Position zero

type Line = { p1 ∷ Position, p2 ∷ Position }

