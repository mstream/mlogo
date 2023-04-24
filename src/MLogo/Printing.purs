module MLogo.Printing (printExpressions) where

import Prelude

import Control.Lazy as Lazy
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.List (List)
import Data.Newtype (unwrap)
import Data.Number.Format as NumberFormat
import Data.String as String
import MLogo.Lexing as Lexing
import MLogo.Parsing
  ( Expression(..)
  , ForBlockSpec
  , Parameter
  , ProcedureSignature
  )

printExpressions ∷ ∀ f. Foldable f ⇒ f Expression → String
printExpressions = foldl f ""
  where
  f ∷ String → Expression → String
  f acc expression = acc <> " " <> printExpression expression

printExpression ∷ Expression → String
printExpression = case _ of
  Addition leftOperand rightOperand →
    printAddition { leftOperand, rightOperand }
  BooleanLiteral b →
    if b then "true" else "false"
  Division leftOperand rightOperand →
    printDivision { leftOperand, rightOperand }
  Equation leftOperand rightOperand →
    printEquation { leftOperand, rightOperand }
  Exponentiation leftOperand rightOperand →
    printExponentiation { leftOperand, rightOperand }
  FloatLiteral x →
    NumberFormat.toString x
  ForBlock spec body →
    printForBlock { body, spec }
  IfBlock condition positiveBranch →
    printIfBlock { condition, positiveBranch }
  IfElseBlock condition positiveBranch negativeBranch →
    printIfElseBlock { condition, positiveBranch, negativeBranch }
  IntegerLiteral n →
    show n
  Multiplication leftOperand rightOperand →
    printMultiplication { leftOperand, rightOperand }
  ProcedureCall name arguments →
    printProcedureCall { arguments, name }
  ProcedureDefinition { name, parameters } body →
    printProcedureDefinition { body, name, parameters }
  RepeatBlock times body →
    printRepeatBlock { body, times }
  StringLiteral s →
    "\"" <> s
  SubExpression expression →
    String.joinWith " " [ "(", printExpression expression, ")" ]
  ValueReference name →
    printValueReference name
  VariableAssignment name value →
    printVariableAssignment { name, value }

printAddition
  ∷ { leftOperand ∷ Expression, rightOperand ∷ Expression } → String
printAddition = Lazy.defer \_ →
  printBinaryOperation Lexing.asteriskSymbol

printDivision
  ∷ { leftOperand ∷ Expression, rightOperand ∷ Expression } → String
printDivision = Lazy.defer \_ →
  printBinaryOperation Lexing.slashSymbol

printEquation
  ∷ { leftOperand ∷ Expression, rightOperand ∷ Expression } → String
printEquation = Lazy.defer \_ →
  printBinaryOperation Lexing.equalSymbol

printExponentiation
  ∷ { leftOperand ∷ Expression, rightOperand ∷ Expression } → String
printExponentiation = Lazy.defer \_ →
  printBinaryOperation Lexing.caretSymbol

printMultiplication
  ∷ { leftOperand ∷ Expression, rightOperand ∷ Expression } → String
printMultiplication = Lazy.defer \_ →
  printBinaryOperation Lexing.asteriskSymbol

printBinaryOperation
  ∷ String
  → { leftOperand ∷ Expression, rightOperand ∷ Expression }
  → String
printBinaryOperation operator { leftOperand, rightOperand } =
  String.joinWith " "
    [ printExpression leftOperand
    , operator
    , printExpression rightOperand
    ]

printForBlock ∷ { body ∷ List Expression, spec ∷ ForBlockSpec } → String
printForBlock { body, spec } =
  String.joinWith " "
    [ Lexing.forKeyword
    , "["
    , printForBlockSpec spec
    , "]"
    , "["
    , printExpressions body
    , "]"
    ]

printForBlockSpec ∷ ForBlockSpec → String
printForBlockSpec { binder, initialValue, step, terminalValue } =
  String.joinWith " "
    ( [ binder
      , show initialValue
      , show terminalValue
      ] <> if step > 1 then [ show step ] else []
    )

printIfBlock
  ∷ { condition ∷ Expression
    , positiveBranch ∷ List Expression
    }
  → String
printIfBlock { condition, positiveBranch } =
  String.joinWith " "
    [ Lexing.ifKeyword
    , "["
    , printExpression condition
    , "]"
    , "["
    , printExpressions positiveBranch
    , "]"
    ]

printIfElseBlock
  ∷ { condition ∷ Expression
    , positiveBranch ∷ List Expression
    , negativeBranch ∷ List Expression
    }
  → String
printIfElseBlock { condition, negativeBranch, positiveBranch } =
  String.joinWith " "
    [ Lexing.ifElseKeyword
    , "["
    , printExpression condition
    , "]"
    , "["
    , printExpressions positiveBranch
    , "]"
    , "["
    , printExpressions negativeBranch
    , "]"
    ]

printProcedureCall
  ∷ { arguments ∷ List Expression, name ∷ String } → String
printProcedureCall { arguments, name } =
  String.joinWith " "
    [ name
    , String.joinWith
        " "
        (Array.fromFoldable $ printExpression <$> arguments)
    ]

printProcedureDefinition
  ∷ { body ∷ List Expression
    , name ∷ String
    , parameters ∷ List Parameter
    }
  → String
printProcedureDefinition { body, name, parameters } =
  String.joinWith "\n"
    [ printProcedureSignature { name, parameters }
    , printExpressions body
    , Lexing.endKeyword
    ]

printProcedureSignature ∷ ProcedureSignature → String
printProcedureSignature { name, parameters } =
  String.joinWith " "
    [ Lexing.toKeyword
    , name
    , String.joinWith " "
        (Array.fromFoldable $ ((":" <> _) <<< unwrap) <$> parameters)
    ]

printRepeatBlock
  ∷ { body ∷ List Expression
    , times ∷ Expression
    }
  → String
printRepeatBlock { body, times } =
  String.joinWith " "
    [ Lexing.repeatKeyword
    , printExpression times
    , "["
    , printExpressions body
    , "]"
    ]

printValueReference ∷ String → String
printValueReference name = ":" <> name

printVariableAssignment ∷ { name ∷ String, value ∷ Expression } → String
printVariableAssignment { name, value } =
  String.joinWith " "
    [ Lexing.makeKeyword
    , name
    , printExpression value
    ]
