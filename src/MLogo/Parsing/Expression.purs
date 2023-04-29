module MLogo.Parsing.Expression
  ( Expression(..)
  , ForBlockSpec
  , ParameterName(..)
  , ProcedureSignature
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary)

data Expression
  = Addition Expression Expression
  | BooleanLiteral Boolean
  | Division Expression Expression
  | Equation Expression Expression
  | Exponentiation Expression Expression
  | IfBlock Expression (List Expression)
  | IfElseBlock Expression (List Expression) (List Expression)
  | IntegerLiteral Int
  | FloatLiteral Number
  | ForBlock ForBlockSpec (List Expression)
  | Multiplication Expression Expression
  | ProcedureCall String (List Expression)
  | ProcedureDefinition ProcedureSignature (List Expression)
  | RepeatBlock Expression (List Expression)
  | StringLiteral String
  | SubExpression Expression
  | Subtraction Expression Expression
  | ValueReference String
  | VariableAssignment String Expression

type ProcedureSignature =
  { name ∷ String
  , parameterNames ∷ List ParameterName
  }

type ForBlockSpec =
  { binder ∷ String
  , initialValue ∷ Int
  , step ∷ Int
  , terminalValue ∷ Int
  }

derive instance Generic Expression _

derive instance Eq Expression

instance Show Expression where
  show s = genericShow s

newtype ParameterName = ParameterName String

derive newtype instance Eq ParameterName
derive newtype instance Ord ParameterName
derive newtype instance Show ParameterName
derive newtype instance Arbitrary ParameterName

derive instance Newtype ParameterName _
