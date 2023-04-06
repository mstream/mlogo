module MLogo.Parsing
  ( Expression(..)
  , Parameter(..)
  , ProcedureCall(..)
  , Statement(..)
  , run
  ) where

import Prelude

import Control.Lazy as Lazy
import Data.Either.Nested (type (\/))
import Data.Function.Uncurried (mkFn5, runFn2)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import MLogo.Lexing (Token(..))
import Parsing
  ( ParseError(..)
  , ParseState(..)
  , Parser
  , ParserT(..)
  , Position(..)
  )
import Parsing (runParser) as P
import Parsing.Combinators (choice, many) as P

type TokenParser a = Parser (List Token) a
type ProgramParser = TokenParser (List Statement)

data Statement
  = ProcedureCallStatement ProcedureCall
  | ProcedureDefinition String (List Parameter) (List Statement)

derive instance Generic Statement _
derive instance Eq Statement

instance Show Statement where
  show statement = genericShow statement

newtype Parameter = Parameter String

derive newtype instance Eq Parameter
derive newtype instance Ord Parameter
derive newtype instance Show Parameter

data Expression
  = ListLiteral (List Expression)
  | NumericLiteral Int
  | ProcedureCallExpression ProcedureCall
  | VariableReference String
  | WordLiteral String

derive instance Generic Expression _
derive instance Eq Expression

instance Show Expression where
  show expression = genericShow expression

data ProcedureCall = ProcedureCall String (List Expression)

derive instance Generic ProcedureCall _
derive instance Eq ProcedureCall

instance Show ProcedureCall where
  show = genericShow

run ∷ List Token → ParseError \/ List Statement
run tokens = P.runParser tokens programParser

programParser ∷ ProgramParser
programParser = P.many statementParser

statementParser ∷ TokenParser Statement
statementParser = Lazy.defer \_ → P.choice
  [ procedureCallStatementParser
  , procedureDefinitionParser
  ]

procedureCallStatementParser ∷ TokenParser Statement
procedureCallStatementParser = do
  name ← consumeUnquotedWord \s → s /= "to" && s /= "end"
  args ← P.many expressionParser
  pure $ ProcedureCallStatement $ ProcedureCall name args

procedureDefinitionParser ∷ TokenParser Statement
procedureDefinitionParser = do
  void $ consumeUnquotedWord (_ == "to")
  name ← consumeUnquotedWord \s → s /= "to" && s /= "end"
  params ← P.many consumeColonPrefixedWord
  body ← P.many statementParser
  void $ consumeUnquotedWord (_ == "end")
  pure $ ProcedureDefinition name (Parameter <$> params) body

expressionParser ∷ TokenParser Expression
expressionParser = P.choice
  [ numericLiteralParser
  , variableReferenceParser
  , wordLiteralParser
  ]

numericLiteralParser ∷ TokenParser Expression
numericLiteralParser = NumericLiteral <$> consumeNumber

variableReferenceParser ∷ TokenParser Expression
variableReferenceParser = VariableReference <$> consumeColonPrefixedWord

wordLiteralParser ∷ TokenParser Expression
wordLiteralParser = WordLiteral <$> consumeQuotedWord

consumeQuotedWord ∷ TokenParser String
consumeQuotedWord = consumeToken case _ of
  QuotedWord s →
    Just s
  _ → Nothing

consumeUnquotedWord ∷ (String → Boolean) → TokenParser String
consumeUnquotedWord predicate = consumeToken case _ of
  UnquotedWord s →
    if predicate s then Just s else Nothing
  _ →
    Nothing

consumeColonPrefixedWord ∷ TokenParser String
consumeColonPrefixedWord = consumeToken case _ of
  ColonPrefixedWord s →
    Just s
  _ →
    Nothing

consumeNumber ∷ TokenParser Int
consumeNumber = consumeToken case _ of
  Number n →
    Just n
  _ → Nothing

consumeToken ∷ ∀ a. (Token → Maybe a) → TokenParser a
consumeToken f = ParserT
  ( mkFn5 \state@(ParseState input pos _) _ _ throw done →
      case List.uncons input of
        Nothing →
          runFn2 throw state (ParseError "Unexpected EOF" pos)
        Just { head, tail } →
          case f head of
            Just ast →
              let
                (Position p) = pos
              in
                runFn2 done
                  ( ParseState tail
                      (Position $ p { index = p.index + 1 })
                      true
                  )
                  ast
            Nothing →
              runFn2 throw state
                (ParseError "Predicate unsatisfied" pos)
  )

