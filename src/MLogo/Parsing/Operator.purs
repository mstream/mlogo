module MLogo.Parsing.Operator (operatorTable) where

import Prelude

import Data.Identity (Identity)
import MLogo.Lexing as Lexing
import MLogo.Parsing.Expression (Expression(..))
import Parsing.Combinators as PC
import Parsing.Expr (Assoc(..), Operator(..))
import Parsing.String as PS

operatorTable ∷ Array (Array (Operator Identity String Expression))
operatorTable =
  [ equationOperator
  , exponentiationOperator
  , divisionOperator <> multiplicationOperator
  , additionOperator <> subtractionOperator
  ]

additionOperator ∷ Array (Operator Identity String Expression)
additionOperator = arithmeticalBinaryOperator
  Lexing.plusSymbol
  Addition
  AssocLeft

divisionOperator ∷ Array (Operator Identity String Expression)
divisionOperator = arithmeticalBinaryOperator
  Lexing.slashSymbol
  Division
  AssocLeft

equationOperator ∷ Array (Operator Identity String Expression)
equationOperator = arithmeticalBinaryOperator
  Lexing.equalSymbol
  Equation
  AssocLeft

exponentiationOperator ∷ Array (Operator Identity String Expression)
exponentiationOperator = arithmeticalBinaryOperator
  Lexing.caretSymbol
  Exponentiation
  AssocRight

multiplicationOperator ∷ Array (Operator Identity String Expression)
multiplicationOperator = arithmeticalBinaryOperator
  Lexing.asteriskSymbol
  Multiplication
  AssocLeft

subtractionOperator ∷ Array (Operator Identity String Expression)
subtractionOperator =
  [ Infix parser AssocLeft ]
  where
  parser = PC.try $ Subtraction
    <$ PC.between
      Lexing.lexer.whiteSpace
      (PC.skipMany1 $ PS.string " ")
      (PS.string Lexing.minusSymbol)

arithmeticalBinaryOperator
  ∷ String
  → (Expression → Expression → Expression)
  → Assoc
  → Array (Operator Identity String Expression)
arithmeticalBinaryOperator symbol constructor assoc =
  [ Infix parser assoc ]
  where
  parser = PC.try $ constructor
    <$ PC.between
      Lexing.lexer.whiteSpace
      Lexing.lexer.whiteSpace
      (PS.string symbol)

