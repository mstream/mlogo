module MLogo.Parsing.Operator
  ( Associativity(..)
  , associativity
  , isAssociative
  , operatorTable
  , precedenceComparingTo
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)
import MLogo.Lexing as Lexing
import MLogo.Parsing.Expression
  ( BinaryOperationType(..)
  , Expression(..)
  , UnaryOperationType(..)
  )
import Parsing (Parser)
import Parsing.Combinators as PC
import Parsing.Expr (Assoc(..), Operator(..))
import Parsing.String as PS

newtype Associativity = Associativity Assoc

derive instance Newtype Associativity _

instance Eq Associativity where
  eq = case _, _ of
    Associativity AssocLeft, Associativity AssocLeft →
      true
    Associativity AssocNone, Associativity AssocNone →
      true
    Associativity AssocRight, Associativity AssocRight →
      true
    _, _ →
      false

associativity ∷ BinaryOperationType → Associativity
associativity = Associativity <<< case _ of
  Addition →
    AssocLeft
  Division →
    AssocLeft
  Equation →
    AssocLeft
  Exponentiation →
    AssocRight
  Multiplication →
    AssocLeft
  Subtraction →
    AssocLeft

isAssociative ∷ BinaryOperationType → Boolean
isAssociative = case _ of
  Addition →
    true
  Division →
    false
  Equation →
    true
  Exponentiation →
    false
  Multiplication →
    true
  Subtraction →
    false

precedenceComparingTo
  ∷ BinaryOperationType → BinaryOperationType → Ordering
precedenceComparingTo = case _, _ of
  Addition, Addition →
    EQ
  Addition, Division →
    LT
  Addition, Equation →
    GT
  Addition, Exponentiation →
    LT
  Addition, Multiplication →
    LT
  Addition, Subtraction →
    EQ
  Division, Addition →
    GT
  Division, Division →
    EQ
  Division, Equation →
    GT
  Division, Exponentiation →
    LT
  Division, Multiplication →
    EQ
  Division, Subtraction →
    GT
  Equation, Addition →
    LT
  Equation, Division →
    LT
  Equation, Equation →
    EQ
  Equation, Exponentiation →
    LT
  Equation, Multiplication →
    LT
  Equation, Subtraction →
    LT
  Exponentiation, Addition →
    GT
  Exponentiation, Division →
    GT
  Exponentiation, Equation →
    GT
  Exponentiation, Exponentiation →
    EQ
  Exponentiation, Multiplication →
    GT
  Exponentiation, Subtraction →
    GT
  Multiplication, Addition →
    GT
  Multiplication, Division →
    EQ
  Multiplication, Equation →
    GT
  Multiplication, Exponentiation →
    LT
  Multiplication, Multiplication →
    EQ
  Multiplication, Subtraction →
    GT
  Subtraction, Addition →
    EQ
  Subtraction, Division →
    LT
  Subtraction, Equation →
    GT
  Subtraction, Exponentiation →
    LT
  Subtraction, Multiplication →
    LT
  Subtraction, Subtraction →
    EQ

operatorTable ∷ Array (Array (Operator Identity String Expression))
operatorTable = unaryOperatorTable <> binaryOperatorTable
  where
  unaryOperatorTable
    ∷ Array (Array (Operator Identity String Expression))
  unaryOperatorTable = unaryOpsByPriority
    <#> map \{ operationType, symbol } →
      Prefix $ unaryParser operationType symbol

  binaryOperatorTable
    ∷ Array (Array (Operator Identity String Expression))
  binaryOperatorTable = binaryOpsByPriority
    <#> map \{ operationType, symbol } →
      Infix
        (binaryParser operationType symbol)
        (unwrap $ associativity operationType)

  unaryOpsByPriority ∷ Array (Array UnaryOperatorExpression)
  unaryOpsByPriority =
    [ Array.fromFoldable unaryOperatorExpressionsTable ]

  binaryOpsByPriority ∷ Array (Array BinaryOperatorExpression)
  binaryOpsByPriority = Array.fromFoldable
    <$> Array.groupAllBy
      (\l r → r.operationType `precedenceComparingTo` l.operationType)
      binaryOperatorExpressionsTable

binaryParser
  ∷ BinaryOperationType
  → String
  → Parser String (Expression → Expression → Expression)
binaryParser operationType symbol = PC.try $
  (BinaryOperation operationType)
    <$ PC.between
      Lexing.lexer.whiteSpace
      ( if operationType == Subtraction then void
          $ PC.many1
          $ (PS.string " " <|> PS.string "\n")
        else Lexing.lexer.whiteSpace
      )
      (PS.string symbol)

unaryParser
  ∷ UnaryOperationType
  → String
  → Parser String (Expression → Expression)
unaryParser operationType symbol = PC.try $
  (UnaryOperation operationType)
    <$ PC.between
      Lexing.lexer.whiteSpace
      (PS.string "")
      (PS.string symbol)

type UnaryOperatorExpression =
  { operationType ∷ UnaryOperationType
  , symbol ∷ String
  }

unaryOperatorExpressionsTable ∷ Array UnaryOperatorExpression
unaryOperatorExpressionsTable =
  [ { operationType: Negation
    , symbol: Lexing.minusSymbol
    }
  ]

type BinaryOperatorExpression =
  { operationType ∷ BinaryOperationType
  , symbol ∷ String
  }

binaryOperatorExpressionsTable ∷ Array BinaryOperatorExpression
binaryOperatorExpressionsTable =
  [ { operationType: Equation
    , symbol: Lexing.equalSymbol
    }
  , { operationType: Exponentiation
    , symbol: Lexing.caretSymbol
    }
  , { operationType: Division
    , symbol: Lexing.slashSymbol
    }
  , { operationType: Multiplication
    , symbol: Lexing.asteriskSymbol
    }
  , { operationType: Addition
    , symbol: Lexing.plusSymbol
    }
  , { operationType: Subtraction
    , symbol: Lexing.minusSymbol
    }
  ]

