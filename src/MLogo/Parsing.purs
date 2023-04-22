module MLogo.Parsing
  ( Arity
  , Expression(..)
  , ForBlockSpec
  , Parameter(..)
  , ParsingContext
  , ProcedureSignature
  , expression
  , expressions
  , procedureSignature
  , procedureSignatures
  , procedureSignaturesToParsingContext
  ) where

import Prelude

import Control.Lazy as Lazy
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple as Tuple
import Data.Unfoldable (replicateA)
import MLogo.Lexing as Lexing
import Parsing (Parser)
import Parsing as P
import Parsing.Combinators ((<?>))
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
  | ForBlock ForBlockSpec (List Expression)
  | Multiplication Expression Expression
  | ProcedureCall String (List Expression)
  | ProcedureDefinition ProcedureSignature (List Expression)
  | RepeatBlock Expression (List Expression)
  | StringLiteral String
  | ValueReference String
  | VariableAssignment String Expression

type ProcedureSignature =
  { name ∷ String
  , parameters ∷ List Parameter
  }

procedureSignaturesToParsingContext
  ∷ ∀ f. Foldable f ⇒ f ProcedureSignature → ParsingContext
procedureSignaturesToParsingContext = foldl f Map.empty
  where
  f ∷ ParsingContext → ProcedureSignature → ParsingContext
  f acc { name, parameters } = Map.insert
    name
    (Just $ List.length parameters)
    acc

type ForBlockSpec =
  { binder ∷ String
  , initialValue ∷ Int
  , terminalValue ∷ Int
  }

derive instance Generic Expression _

derive instance Eq Expression

instance Show Expression where
  show s = genericShow s

instance Arbitrary Expression where
  arbitrary = Lazy.defer \_ → genericArbitrary

type ParsingContext = Map String Arity

type Arity = Maybe Int

expressions ∷ ParsingContext → Parser String (List Expression)
expressions context = Lazy.defer \_ →
  Lexing.lexer.whiteSpace *> (PC.many $ expression context)

expression ∷ ParsingContext → Parser String Expression
expression context = Lexing.lexer.whiteSpace *>
  PE.buildExprParser operatorTable term
  where
  term = Lazy.defer \_ → PC.choice
    [ Lexing.lexer.parens $ expression context
    , forBlock context
    , ifBlock context
    , ifElseBlock context
    , literal
    , procedureDefinition context
    , procedureCall context
    , repeatBlock context
    , valueReference
    , variableAssignment context
    , P.fail "could not recognize an expression term"
    ]

forBlock ∷ ParsingContext → Parser String Expression
forBlock context = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.forKeyword
  Lexing.lexer.whiteSpace
  spec ← Lexing.lexer.brackets forBlockSpec
  Lexing.lexer.whiteSpace
  body ← Lexing.lexer.brackets $ expressions context
  pure $ ForBlock spec body

forBlockSpec ∷ Parser String ForBlockSpec
forBlockSpec = do
  binder ← Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  initialValue ← Lexing.lexer.integer
  Lexing.lexer.whiteSpace
  terminalValue ← Lexing.lexer.integer
  pure { binder, initialValue, terminalValue }

ifBlock ∷ ParsingContext → Parser String Expression
ifBlock context = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.ifKeyword
  Lexing.lexer.whiteSpace
  condition ← expression context
  Lexing.lexer.whiteSpace
  positiveBranch ← Lexing.lexer.brackets $ expressions context
  pure $ IfBlock condition positiveBranch

ifElseBlock ∷ ParsingContext → Parser String Expression
ifElseBlock context = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.ifElseKeyword
  Lexing.lexer.whiteSpace
  condition ← expression context
  Lexing.lexer.whiteSpace
  positiveBranch ← Lexing.lexer.brackets $ expressions context
  Lexing.lexer.whiteSpace
  negativeBranch ← Lexing.lexer.brackets $ expressions context
  pure $ IfElseBlock condition positiveBranch negativeBranch

literal ∷ Parser String Expression
literal = PC.choice
  [ FloatLiteral <$> PC.try Lexing.lexer.float
  , IntegerLiteral <$> Lexing.lexer.integer
  , BooleanLiteral true <$ Lexing.lexer.reserved Lexing.trueKeyword
  , BooleanLiteral false <$ Lexing.lexer.reserved Lexing.falseKeyword
  , StringLiteral <$> (PS.string "\"" *> Lexing.lexer.identifier)
  , P.fail "could not recognize a literal"
  ]

procedureCall ∷ ParsingContext → Parser String Expression
procedureCall context = Lazy.defer \_ → do
  name ← Lexing.lexer.identifier
  Lexing.lexer.whiteSpace

  arguments ← case Map.lookup name context of
    Just (Just n) →
      replicateA n (Lexing.lexer.lexeme $ expression context)
    Just Nothing →
      PC.many $ Lexing.lexer.lexeme $ expression context
    Nothing →
      P.fail $ "unknown \"" <> name <> "\" procedure name"

  pure $ ProcedureCall name arguments

procedureDefinition ∷ ParsingContext → Parser String Expression
procedureDefinition context = do
  signature ← procedureSignature
  Lexing.lexer.whiteSpace
  body ← expressions context
  Lexing.lexer.reserved Lexing.endKeyword
  pure $ ProcedureDefinition signature body

procedureSignatures ∷ Parser String (List ProcedureSignature)
procedureSignatures = do
  signatures ← PC.many $
    Tuple.snd <$> (PC.try $ PS.anyTill procedureSignature)
  void PS.rest
  pure signatures

procedureSignature ∷ Parser String ProcedureSignature
procedureSignature = Lexing.lexer.whiteSpace *> do
  Lexing.lexer.reserved Lexing.toKeyword
    <?> "procedure signature should start with \"to\" keyword"
  name ← Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  parameters ← PC.many $ Lexing.lexer.lexeme parameter
  pure { name, parameters }

parameter ∷ Parser String Parameter
parameter = do
  void $ PS.string ":"
    <?> "a colon prefixing the parameter"
  name ← Lexing.lexer.identifier
  pure $ Parameter name

repeatBlock ∷ ParsingContext → Parser String Expression
repeatBlock context = Lazy.defer \_ → do
  Lexing.lexer.reserved Lexing.repeatKeyword
  Lexing.lexer.whiteSpace
  times ← expression context
  Lexing.lexer.whiteSpace
  body ← Lexing.lexer.brackets $ expressions context
  pure $ RepeatBlock times body

valueReference ∷ Parser String Expression
valueReference = do
  void $ PS.string ":"
    <?> "a colon prefixing the value reference"
  name ← Lexing.lexer.identifier
  pure $ ValueReference name

variableAssignment ∷ ParsingContext → Parser String Expression
variableAssignment context = do
  Lexing.lexer.reserved Lexing.makeKeyword
  Lexing.lexer.whiteSpace
  name ← PS.string "\"" *> Lexing.lexer.identifier
  Lexing.lexer.whiteSpace
  value ← expression context
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
