module MLogo.Parsing
  ( Expression(..)
  , Parameter(..)
  , expression
  , expressions
  ) where

import Prelude

import Control.Lazy as Lazy
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List)
import Data.Show.Generic (genericShow)
import MLogo.Lexing as Lexing
import Parsing (Parser)
import Parsing.Combinators as PC
import Parsing.Expr (Assoc(..), Operator(..))
import Parsing.Expr as PE
import Parsing.String as PS
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

data Expression
  = Addition Expression Expression
  | BooleanLiteral Boolean
  | Equation Expression Expression
  | IfBlock Expression (List Expression)
  | IfElseBlock Expression (List Expression) (List Expression)
  | IntegerLiteral Int
  | FloatLiteral Number
  | Multiplication Expression Expression
  | ProcedureCall String (List Expression)
  | ProcedureDefinition String (List Parameter) (List Expression)
  | RepeatBlock Expression (List Expression)
  | StringLiteral String
  | ValueReference String
  | VariableAssignment String Expression

derive instance Generic Expression _

derive instance Eq Expression

instance Show Expression where
  show s = genericShow s

instance Arbitrary Expression where
  arbitrary = Lazy.defer \_ → genericArbitrary

expressions ∷ Parser String (List Expression)
expressions = Lazy.defer \_ →
  Lexing.lexer.whiteSpace *> PC.many expression

expression ∷ Parser String Expression
expression = Lexing.lexer.whiteSpace *>
  PE.buildExprParser operatorTable term
  where
  term = Lazy.defer \_ → PC.choice
    [ Lexing.lexer.parens expression
    , ifBlock
    , ifElseBlock
    , literal
    , procedureDefinition
    , procedureCall
    , repeatBlock
    , valueReference
    , variableAssignment
    ]

ifBlock ∷ Parser String Expression
ifBlock = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.ifKeyword
  Lexing.lexer.whiteSpace
  condition ← expression
  Lexing.lexer.whiteSpace
  positiveBranch ← Lexing.lexer.brackets expressions
  pure $ IfBlock condition positiveBranch

ifElseBlock ∷ Parser String Expression
ifElseBlock = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.ifElseKeyword
  Lexing.lexer.whiteSpace
  condition ← argument
  Lexing.lexer.whiteSpace
  positiveBranch ← Lexing.lexer.brackets expressions
  Lexing.lexer.whiteSpace
  negativeBranch ← Lexing.lexer.brackets expressions
  pure $ IfElseBlock condition positiveBranch negativeBranch

literal ∷ Parser String Expression
literal = PC.choice
  [ FloatLiteral <$> PC.try Lexing.lexer.float
  , IntegerLiteral <$> Lexing.lexer.integer
  , BooleanLiteral true <$ Lexing.lexer.reserved Lexing.trueKeyword
  , BooleanLiteral false <$ Lexing.lexer.reserved Lexing.falseKeyword
  , StringLiteral <$> (PS.string "\"" *> Lexing.lexer.identifier)
  ]

procedureCall ∷ Parser String Expression
procedureCall = Lazy.defer \_ → do
  name ← Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  arguments ← PC.many $ Lexing.lexer.lexeme argument
  pure $ ProcedureCall name arguments

argument ∷ Parser String Expression
argument = PE.buildExprParser operatorTable term
  where
  term = Lazy.defer \_ → PC.choice
    [ Lexing.lexer.parens argument
    , literal
    , Lexing.lexer.parens procedureCall
    , valueReference
    ]

procedureDefinition ∷ Parser String Expression
procedureDefinition = do
  Lexing.lexer.reserved Lexing.toKeyword
  name ← Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  parameters ← PC.many $ Lexing.lexer.lexeme parameter
  Lexing.lexer.whiteSpace
  body ← expressions
  Lexing.lexer.reserved Lexing.endKeyword
  pure $ ProcedureDefinition name parameters body

parameter ∷ Parser String Parameter
parameter = do
  void $ PS.string ":"
  name ← Lexing.lexer.identifier
  pure $ Parameter name

repeatBlock ∷ Parser String Expression
repeatBlock = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.repeatKeyword
  Lexing.lexer.whiteSpace
  times ← argument
  Lexing.lexer.whiteSpace
  body ← Lexing.lexer.brackets expressions
  pure $ RepeatBlock times body

valueReference ∷ Parser String Expression
valueReference = do
  void $ PS.string ":"
  name ← Lexing.lexer.identifier
  pure $ ValueReference name

variableAssignment ∷ Parser String Expression
variableAssignment = do
  Lexing.lexer.reserved Lexing.makeKeyword
  Lexing.lexer.whiteSpace
  name ← PS.string "\"" *> Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  value ← argument
  pure $ VariableAssignment name value

arithmeticalBinaryOperator
  ∷ String
  → (Expression → Expression → Expression)
  → Assoc
  → Array (Operator Identity String Expression)
arithmeticalBinaryOperator symbol constructor assoc =
  [ Infix
      ( constructor <$ PC.between
          (PC.optional Lexing.lexer.whiteSpace)
          (PC.optional Lexing.lexer.whiteSpace)
          (PS.string symbol)
      )
      assoc
  ]

newtype Parameter = Parameter String

derive newtype instance Eq Parameter
derive newtype instance Ord Parameter
derive newtype instance Show Parameter
derive newtype instance Arbitrary Parameter

operatorTable ∷ Array (Array (Operator Identity String Expression))
operatorTable =
  [ arithmeticalBinaryOperator
      Lexing.plusSymbol
      Addition
      AssocRight
  , arithmeticalBinaryOperator
      Lexing.equalSymbol
      Equation
      AssocNone
  , arithmeticalBinaryOperator
      Lexing.asteriskSymbol
      Multiplication
      AssocRight
  ]
