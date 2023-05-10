module MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , ForBlockSpec
  , ParameterName(..)
  , ProcedureSignature
  , UnaryOperationType(..)
  , binaryOperationTypeSymbol
  , unaryOperationTypeSymbol
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Encode.Generic as AEG
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import MLogo.Lexing as Lexing
import Test.QuickCheck (class Arbitrary)

data Expression
  = BinaryOperation BinaryOperationType Expression Expression
  | BooleanLiteral Boolean
  | IfBlock Expression (List Expression)
  | IfElseBlock Expression (List Expression) (List Expression)
  | IntegerLiteral Int
  | FloatLiteral Number
  | ForBlock ForBlockSpec (List Expression)
  | ProcedureCall String (List Expression)
  | ProcedureDefinition ProcedureSignature (List Expression)
  | RepeatBlock Expression (List Expression)
  | StringLiteral String
  | UnaryOperation UnaryOperationType Expression
  | ValueReference String
  | VariableAssignment String Expression

data BinaryOperationType
  = Addition
  | Division
  | Equation
  | Exponentiation
  | Multiplication
  | Subtraction

derive instance Eq BinaryOperationType
derive instance Generic BinaryOperationType _

instance EncodeJson BinaryOperationType where
  encodeJson = genericEncodeJson

instance Show BinaryOperationType where
  show = genericShow

binaryOperationTypeSymbol ∷ BinaryOperationType → String
binaryOperationTypeSymbol = case _ of
  Addition →
    Lexing.plusSymbol
  Division →
    Lexing.slashSymbol
  Equation →
    Lexing.equalSymbol
  Exponentiation →
    Lexing.caretSymbol
  Multiplication →
    Lexing.asteriskSymbol
  Subtraction →
    Lexing.minusSymbol

data UnaryOperationType = Negation

derive instance Eq UnaryOperationType
derive instance Generic UnaryOperationType _

instance EncodeJson UnaryOperationType where
  encodeJson = genericEncodeJson

instance Ord UnaryOperationType where
  compare = case _, _ of
    Negation, Negation →
      EQ

instance Show UnaryOperationType where
  show = genericShow

unaryOperationTypeSymbol ∷ UnaryOperationType → String
unaryOperationTypeSymbol = case _ of
  Negation →
    Lexing.minusSymbol

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
  show expr = genericShow expr

newtype ParameterName = ParameterName String

derive newtype instance Arbitrary ParameterName
derive newtype instance EncodeJson ParameterName
derive newtype instance Eq ParameterName
derive newtype instance Ord ParameterName
derive newtype instance Show ParameterName

derive instance Newtype ParameterName _

instance EncodeJson Expression where
  encodeJson expr = AEG.genericEncodeJson expr
