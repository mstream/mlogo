module MLogo.Interpretation.State
  ( Angle(..)
  , CallStackElement
  , ExecutionState
  , Line
  , PointerState
  , Position(..)
  , Procedure
  , ScreenState
  , Value(..)
  , Variables
  , VisibleState
  , extractBoolean
  , extractInt
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
import Data.Int as Int
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import MLogo.Parsing.Expression (Expression, ParameterName)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

type Variables = Map String Value

data Value
  = BooleanValue Boolean
  | FloatValue Number
  | IntegerValue Int
  {- TODO: implement  | ListValue (List Value) -}
  | WordValue String

derive instance Generic Value _
derive instance Eq Value

instance Show Value where
  show v = genericShow v

instance Arbitrary Value where
  arbitrary = genericArbitrary

newtype Angle = Angle Number

derive newtype instance Eq Angle
derive newtype instance Show Angle
derive newtype instance Semiring Angle
derive newtype instance Ring Angle
derive newtype instance EncodeJson Angle
derive newtype instance Arbitrary Angle

derive instance Newtype Angle _

extractBoolean ∷ Value → String \/ Boolean
extractBoolean = case _ of
  BooleanValue b →
    Right b
  otherValue →
    Left $ "\"" <> show otherValue <> "\" is not a boolean value"

extractInt ∷ Value → String \/ Int
extractInt = case _ of
  IntegerValue n →
    Right n
  otherValue →
    Left $ "\"" <> show otherValue <> "\" is not an integer value"

extractNumber ∷ Value → String \/ Number
extractNumber = case _ of
  FloatValue x →
    Right x
  IntegerValue n →
    Right $ Int.toNumber n
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
  { callStack ∷ List CallStackElement
  , globalVariables ∷ Variables
  , outputtedValue ∷ Maybe Value
  , pointer ∷ PointerState
  , procedures ∷ Map String Procedure
  , repCount ∷ Int
  , screen ∷ ScreenState
  }

type Procedure =
  { body ∷ List Expression, parameterNames ∷ List ParameterName }

type CallStackElement =
  { name ∷ String
  , localVariables ∷ Variables
  }

type VisibleState =
  { pointer ∷ PointerState
  , screen ∷ ScreenState
  }

initialExecutionState ∷ ExecutionState
initialExecutionState =
  { callStack: Nil
  , globalVariables: Map.empty
  , outputtedValue: Nothing
  , pointer: initialPointerState
  , procedures: Map.empty
  , repCount: -1
  , screen: Nil
  }

newtype Position = Position { x ∷ Number, y ∷ Number }

derive instance Generic Position _
derive newtype instance Eq Position
derive newtype instance Show Position
derive newtype instance EncodeJson Position
derive newtype instance Arbitrary Position

derive instance Newtype Position _

instance Semiring Position where
  add (Position p1) (Position p2) = Position
    { x: p1.x + p2.x, y: p1.y + p2.y }
  mul (Position p1) (Position p2) = Position
    { x: p1.x * p2.x, y: p1.y * p2.y }
  one = Position one
  zero = Position zero

type Line = { p1 ∷ Position, p2 ∷ Position }
