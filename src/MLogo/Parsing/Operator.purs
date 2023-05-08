module MLogo.Parsing.Operator (isLowerPriorityThan, operatorTable) where

import Prelude

import Data.Array as Array
import Data.Identity (Identity)
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

isLowerPriorityThan
  ∷ BinaryOperationType → BinaryOperationType → Boolean
isLowerPriorityThan = (>)

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
    <#> map \{ associativity, operationType, symbol } →
      Infix (binaryParser operationType symbol) associativity

  unaryOpsByPriority ∷ Array (Array UnaryOperatorExpression)
  unaryOpsByPriority = Array.fromFoldable
    <$> Array.groupAllBy
      (\l r → compare l.operationType r.operationType)
      unaryOperatorExpressionsTable

  binaryOpsByPriority ∷ Array (Array BinaryOperatorExpression)
  binaryOpsByPriority = Array.fromFoldable
    <$> Array.groupAllBy
      (\l r → compare l.operationType r.operationType)
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
          $ PS.string " "
        else Lexing.lexer.whiteSpace
      )
      (PS.string symbol)

unaryParser
  ∷ UnaryOperationType
  → String
  → Parser String (Expression → Expression)
unaryParser operationType symbol = PC.try $
  (UnaryOperation operationType)
    <$ PS.string symbol

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
  { associativity ∷ Assoc
  , operationType ∷ BinaryOperationType
  , symbol ∷ String
  }

binaryOperatorExpressionsTable ∷ Array BinaryOperatorExpression
binaryOperatorExpressionsTable =
  [ { associativity: AssocLeft
    , operationType: Equation
    , symbol: Lexing.equalSymbol
    }
  , { associativity: AssocRight
    , operationType: Exponentiation
    , symbol: Lexing.caretSymbol
    }
  , { associativity: AssocLeft
    , operationType: Division
    , symbol: Lexing.slashSymbol
    }
  , { associativity: AssocLeft
    , operationType: Multiplication
    , symbol: Lexing.asteriskSymbol
    }
  , { associativity: AssocLeft
    , operationType: Addition
    , symbol: Lexing.plusSymbol
    }
  , { associativity: AssocLeft
    , operationType: Subtraction
    , symbol: Lexing.minusSymbol
    }
  ]

